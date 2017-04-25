-module(volmgr_db_people).

-export([create/4, create/5, create/6,
         retrieve/0, retrieve/1,
         retrieve_by_tag/1
        ]).

-include_lib("stdlib/include/qlc.hrl").
-include("types.hrl").
-include("db.hrl").
-include("entities.hrl").

-spec create(First :: binary(),
                    Last :: binary(),
                    Phone :: phone(),
                    Email :: binary()) -> ok | {aborted, any()}.
create(First, Last, Phone, Email) ->
    create(First, Last, Phone, Email, [], []).

-spec create(First :: binary(),
                    Last :: binary(),
                    Phone :: phone(),
                    Email :: binary(),
                    Notes :: list(binary())) -> ok | {aborted, any()}.
create(First, Last, Phone, Email, Notes) ->
    create(First, Last, Phone, Email, Notes, []).

-spec create(First :: binary(),
                    Last :: binary(),
                    Phone :: phone(),
                    Email :: binary(),
                    Notes :: list(binary()),
                    Tags :: list(tag())) -> ok | {error, notfound} | no_return().
create(First, Last, Phone, Email, Notes, _Tags=[]) ->
    Id = create_id(First, Last),
    F = fun() ->
            Person = #volmgr_people{id=Id,
                                    active=true,
                                    first=First,
                                    last=Last,
                                    phone=Phone,
                                    email=Email,
                                    notes=Notes},
            mnesia:write(Person)
        end,
    mnesia:activity(transaction, F);
create(First, Last, Phone, Email, Notes, Tags) ->
    Id = create_id(First, Last),
    F = fun() ->
            volmgr_db_tags:ensure_transactional(Tags),
            Person = #volmgr_people{id=Id,
                                    active=true,
                                    first=First,
                                    last=Last,
                                    phone=Phone,
                                    email=Email,
                                    notes=Notes},
            mnesia:write(Person),
            TagPersonRecords = [#volmgr_people_tags{volmgr_tags_id=Tag, volmgr_people_id=Id} || Tag <- Tags],
            TagPersonWriter = fun W([]) ->
                                  ok;
                              W([Record|T]) ->
                                  mnesia:write(Record),
                                  W(T)
                              end,
            TagPersonWriter(TagPersonRecords)
        end,
    try
        mnesia:activity(transaction, F)
    catch
        exit:{aborted, notfound} -> {error, notfound}
    end.

-spec create_id(First :: binary(), Last :: binary()) -> binary().
create_id(First, Last) ->
    erlang:iolist_to_binary([Last, $-, First]).

-spec retrieve(Id :: binary()) -> person() | {error, notfound}.
retrieve(Id) ->
    F = fun() ->
            case mnesia:read({volmgr_people, Id}) of
                [VP=#volmgr_people{}] -> to_entity(VP);
                [] -> {error, notfound}
            end
        end,
    mnesia:activity(transaction, F).

-spec retrieve() -> list(person()) | list().
retrieve() ->
    I = fun(VP=#volmgr_people{}, Acc)->
	        [to_entity(VP)|Acc]
	    end,
	F = fun() ->
	        mnesia:foldl(I, [], volmgr_people)
	    end,
	mnesia:activity(transaction, F).

-spec retrieve_by_tag(tag()) -> list(person()) | list().
retrieve_by_tag(Tag) ->
    F = fun() ->
            Q = qlc:q([to_entity(VP) || VPT <- mnesia:table(volmgr_people_tags),
                                          VP <- mnesia:table(volmgr_people),
                                          VPT#volmgr_people_tags.volmgr_tags_id =:= Tag,
                                          VPT#volmgr_people_tags.volmgr_people_id =:= VP#volmgr_people.id]),
            qlc:e(Q)
        end,
    mnesia:activity(transaction, F).

-spec to_entity(#volmgr_people{}) -> #person{}.
to_entity(#volmgr_people{id=Id, active=A, first=F, last=L, phone=P, email=E, notes=N})->
    #person{id=Id, active=A, first=F, last=L, phone=P, email=E, notes=N}.