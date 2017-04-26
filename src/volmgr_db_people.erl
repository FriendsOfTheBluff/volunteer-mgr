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
                    Email :: binary()) ->  ok | {error, atom()} | no_return().
create(First, Last, Phone, Email) ->
    create(First, Last, Phone, Email, [], []).

-spec create(First :: binary(),
                    Last :: binary(),
                    Phone :: phone(),
                    Email :: binary(),
                    Notes :: list(binary())) -> ok | {error, atom()} | no_return().
create(First, Last, Phone, Email, Notes) ->
    create(First, Last, Phone, Email, Notes, []).

-spec create(First :: binary(),
                    Last :: binary(),
                    Phone :: phone(),
                    Email :: binary(),
                    Notes :: list(binary()),
                    Tags :: list(tag())) -> ok | {error, atom()} | no_return().
create(First, Last, Phone, Email, Notes, _Tags=[]) ->
    Id = create_id(Last, First, Email),
    F = fun() ->
            ensure_unique(Id),
            Person = #volmgr_people{id=Id,
                                    active=true,
                                    first=First,
                                    last=Last,
                                    phone=Phone,
                                    email=Email,
                                    notes=Notes},
            mnesia:write(Person)
        end,
    try_create_person(F);
create(First, Last, Phone, Email, Notes, Tags) ->
    Id = create_id(Last, First, Email),
    F = fun() ->
            ensure_unique(Id),
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
    try_create_person(F).

-spec try_create_person(F :: function()) -> ok | {error, atom()} | no_return().
try_create_person(F) ->
    try
        mnesia:activity(transaction, F)
    catch
        exit:{aborted, notfound} -> {error, notfound};
        exit:{aborted, eexists} -> {error, eexists}
    end.

-spec ensure_unique(Id :: binary()) -> ok | no_return().
ensure_unique(Id) ->
    case mnesia:read({volmgr_people, Id}) of
        [] -> ok;
        _ -> mnesia:abort(eexists)
    end.

-spec create_id(Last :: binary(), First :: binary(), Email :: binary()) -> binary().
create_id(Last, First, Email) ->
    L = string:to_lower(binary_to_list(Last)),
    F = string:to_lower(binary_to_list(First)),
    erlang:iolist_to_binary([L, $-, F, $-, Email]).

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
