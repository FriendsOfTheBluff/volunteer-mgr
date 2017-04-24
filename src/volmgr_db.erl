-module(volmgr_db).

-export([start/0,
         init_tables/0,
         init_tables/1,
         init_schema/0,
         create_person/4, create_person/5, create_person/6,
         retrieve_person/1,
         retrieve_people/0,
         create_tag/1,
         create_tags/1,
         retrieve_tag/1,
         retrieve_tags/0,
         ensure_tags/1
        ]).

-include("volunteer_mgr.hrl").

-record(volmgr_people,
        {id :: binary(),
         active :: boolean(),
         first :: binary(),
         last :: binary(),
         phone :: phone(),
         email :: binary(),
         notes :: list(binary())
        }).

-record(volmgr_people_tags,
        {volmgr_tags_id :: atom(),
         volmgr_people_id :: binary()
        }).

-record(volmgr_tags, {id :: atom(), active :: boolean()}).

-spec start() ->
    ok | {timeout, list(term())} | {error, any()}.
start() ->
    mnesia:wait_for_tables(tables(), 5000).

-spec tables() -> list(atom()).
tables() ->
    [volmgr_people, volmgr_people_tags, volmgr_tags].

-spec init_tables() -> ok.
init_tables() ->
    init_tables(ram_copies).

-spec init_tables(disc_copies | ram_copies) -> ok.
init_tables(StorageType) ->
    VolmgrPeopleOpts = [
               {attributes, record_info(fields, volmgr_people)},
               {type, ordered_set},
               {StorageType, [node()]}
              ],
    {atomic, ok} = mnesia:create_table(volmgr_people, VolmgrPeopleOpts),
    VolmgrPeopleTagsOpts = [
               {attributes, record_info(fields, volmgr_people_tags)},
               {type, bag},
               {StorageType, [node()]}
              ],
    {atomic, ok} = mnesia:create_table(volmgr_people_tags, VolmgrPeopleTagsOpts),
    VolmgrTagsOpts = [
               {attributes, record_info(fields, volmgr_tags)},
               {type, set},
               {StorageType, [node()]}
              ],
    {atomic, ok} = mnesia:create_table(volmgr_tags, VolmgrTagsOpts),
    ok.

-spec init_schema() -> ok.
init_schema() ->
    io:format("Creating mnesia schema in: ~p~n", [mnesia:system_info(directory)]),
    handle_create_schema(mnesia:create_schema([node()])).

handle_create_schema(ok) ->
    ok = mnesia:start(),
    ok = volmgr_db:init_tables(disc_copies);
handle_create_schema(Err) ->
    io:format(standard_error, "Create schema unexpected result: ~p~n", [Err]).

-spec create_person(First :: binary(),
                    Last :: binary(),
                    Phone :: {integer(), integer(), integer()},
                    Email :: binary()) -> ok | {aborted, any()}.
create_person(First, Last, Phone, Email) ->
    create_person(First, Last, Phone, Email, [], []).

-spec create_person(First :: binary(),
                    Last :: binary(),
                    Phone :: {integer(), integer(), integer()},
                    Email :: binary(),
                    Notes :: list(binary())) -> ok | {aborted, any()}.
create_person(First, Last, Phone, Email, Notes) ->
    create_person(First, Last, Phone, Email, Notes, []).

-spec create_person(First :: binary(),
                    Last :: binary(),
                    Phone :: {integer(), integer(), integer()},
                    Email :: binary(),
                    Notes :: list(binary()),
                    Tags :: list(atom())) -> ok | {error, notfound} | no_return().
create_person(First, Last, Phone, Email, Notes, _Tags=[]) ->
    Id = create_person_id(First, Last),
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
create_person(First, Last, Phone, Email, Notes, Tags) ->
    Id = create_person_id(First, Last),
    F = fun() ->
            do_ensure_tags(Tags),
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

-spec create_person_id(First :: binary(), Last :: binary()) -> binary().
create_person_id(First, Last) ->
    erlang:iolist_to_binary([Last, $-, First]).

-spec retrieve_person(Id :: binary()) -> person() | {error, notfound}.
retrieve_person(Id) ->
    F = fun() ->
            case mnesia:read({volmgr_people, Id}) of
                [#volmgr_people{id=Id, active=A,
                                first=F, last=L,
                                phone=P, email=E, notes=N}] ->
                    #person{id=Id, active=A,
                            first=F, last=L,
                            phone=P, email=E, notes=N};
                [] ->
                    {error, notfound}
            end
        end,
    mnesia:activity(transaction, F).

-spec retrieve_people() -> list(person()) | list().
retrieve_people() ->
    I = fun(#volmgr_people{id=Id, active=A,
                           first=F, last=L,
                           phone=P, email=E, notes=N}, Acc)->
            Person = #person{id=Id, active=A,
                             first=F, last=L,
                             phone=P, email=E, notes=N},
	        [Person|Acc]
	    end,
	F = fun() ->
	        mnesia:foldl(I, [], volmgr_people)
	    end,
	mnesia:activity(transaction, F).

-spec create_tag(Tag :: atom()) -> ok | {error, any()} | no_return().
create_tag(Tag) ->
    create_tags([Tag]).

-spec create_tags(Tags :: list(atom())) -> ok | {aborted, any()}.
create_tags(Tags) ->
    handle_validate_tags(validate_tags(Tags), Tags).

handle_validate_tags({error, invalid_tag}, _Tags) ->
    {error, invalid_tag};
handle_validate_tags(ok, Tags) ->
    Records = [#volmgr_tags{id=Tag, active=true} || Tag <- Tags],
    Writer = fun W([]) ->
                 ok;
             W([Record|T]) ->
                 mnesia:write(Record),
                 W(T)
             end,
    mnesia:activity(transaction, Writer, [Records], mnesia).

-spec validate_tags(Tags :: list(atom())) -> ok | {error, invalid_tag}.
validate_tags(Tags) ->
    Bad = ['ok', 'error', 'notfound', 'undefined'],
    Pred = fun(T) ->
               lists:member(T, Bad) orelse erl_scan:reserved_word(T)
           end,
    case lists:any(Pred, Tags) of
        true -> {error, invalid_tag};
        false -> ok
    end.

-spec retrieve_tag(Tag :: atom()) -> atom() | {error, notfound}.
retrieve_tag(Tag) ->
    F = fun() ->
            case mnesia:read({volmgr_tags, Tag}) of
                [#volmgr_tags{id=Tag, active=A}] ->
                    {Tag, A};
                [] ->
                    {error, notfound}
            end
        end,
    mnesia:activity(transaction, F).

-spec retrieve_tags() -> list(atom()) | list().
retrieve_tags() ->
    I = fun(#volmgr_tags{id=T, active=A}, Acc)->
	        [{T, A}|Acc]
	    end,
	F = fun() ->
	        mnesia:foldl(I, [], volmgr_tags)
	    end,
	mnesia:activity(transaction, F).

-spec ensure_tags(list(atom())) -> ok | {error, notfound} | {error, {any(), any()}}.
ensure_tags(Tags) ->
    try
        do_ensure_tags(Tags)
    catch
        exit:{aborted, notfound} ->
            {error, notfound};
        ExType:ExReason ->
            {error, {ExType, ExReason}}
    end.

%% private
-spec do_ensure_tags(list(atom())) -> ok | no_return().
do_ensure_tags(Tags) ->
    % NOTE: this is a private function that does not catch any
    % mnesia exit exceptions, so it can be used within a transaction
    % OR by a function that does catch the exit exception
    Reader = fun R([]) ->
                 ok;
             R([Tag|T]) ->
                 case mnesia:read({volmgr_tags, Tag}) of
                     [#volmgr_tags{id=Tag, active=true}] -> R(T);
                     [] -> mnesia:abort(notfound)
                 end
             end,
	mnesia:activity(transaction, Reader, [Tags], mnesia).
