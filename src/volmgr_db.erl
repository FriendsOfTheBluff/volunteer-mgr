-module(volmgr_db).

-export([start/0,
         init_tables/0,
         init_tables/1,
         init_schema/0,
         create_person/5,
         retrieve_person/1,
         retrieve_people/0,
         create_tag/1,
         create_tags/1,
         retrieve_tag/1,
         retrieve_tags/0
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

-record(volmgr_tags, {id :: atom(), active :: boolean()}).

-spec start() ->
    ok | {timeout, list(term())} | {error, any()}.
start() ->
    mnesia:wait_for_tables(tables(), 5000).

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

-spec create_person(First :: binary(),
                    Last :: binary(),
                    Phone :: {integer(), integer(), integer()},
                    Email :: binary(),
                    Notes :: list(binary())) -> ok | {aborted, any()}.
create_person(First, Last, Phone, Email, Notes) ->
    Id = erlang:iolist_to_binary([Last, $-, First]),
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
    mnesia:activity(transaction, F).

-spec retrieve_person(Id :: binary()) -> person() | notfound.
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
                    notfound
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

-spec create_tag(Tag :: atom()) -> ok | {aborted, any()}.
create_tag(notfound) ->
    {aborted, <<"invalid tag: notfound">>};
create_tag(Tag) ->
    F = fun() ->
            TagR = #volmgr_tags{id=Tag, active=true},
            mnesia:write(TagR)
        end,
    mnesia:activity(transaction, F).

-spec create_tags(Tags :: list(atom())) -> ok | {aborted, any()}.
create_tags(Tags) ->
    Records = [#volmgr_tags{id=Tag, active=true} || Tag <- Tags],
    Writer = fun W([]) ->
                 ok;
             W([Record|T]) ->
                 mnesia:write(Record),
                 W(T)
             end,
    mnesia:activity(transaction, Writer, [Records], mnesia).

-spec retrieve_tag(Tag :: atom()) -> atom() | notfound.
retrieve_tag(Tag) ->
    F = fun() ->
            case mnesia:read({volmgr_tags, Tag}) of
                [#volmgr_tags{id=Tag, active=A}] ->
                    {Tag, A};
                [] ->
                    notfound
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

%% private
-spec tables() -> list(atom()).
tables() ->
    [volmgr_people, volmgr_tags].

handle_create_schema(ok) ->
    ok = mnesia:start(),
    handle_init_tables(volmgr_db:init_tables(disc_copies));
handle_create_schema(Err) ->
    io:format(standard_error, "Create schema unexpected result: ~p~n", [Err]).

handle_init_tables(ok) ->
    ok;
handle_init_tables(Err) ->
    io:format(standard_error, "Init tables unexpected result: ~p~n", [Err]).
