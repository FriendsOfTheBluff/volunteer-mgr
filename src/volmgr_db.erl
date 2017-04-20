-module(volmgr_db).

-export([tables/0,
         install/1,
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

-spec tables() -> list(atom()).
tables() ->
    [volmgr_people, volmgr_tags].

-spec install(list(node())) -> ok | {error, any()}.
install(Nodes) ->
    ok = mnesia:create_schema(Nodes),

    % TODO multicall rval
    rpc:multicall(Nodes, application, start, [mnesia]),

    VolmgrPeopleOpts = [
               {attributes, record_info(fields, volmgr_people)},
               {disc_copies, Nodes},
               {type, ordered_set}
              ],
    {atomic, ok} = mnesia:create_table(volmgr_people, VolmgrPeopleOpts),

    VolmgrTagsOpts = [
               {attributes, record_info(fields, volmgr_tags)},
               {disc_copies, Nodes},
               {type, set}
              ],
    {atomic, ok} = mnesia:create_table(volmgr_tags, VolmgrTagsOpts),

    % TODO multicall rval
    rpc:multicall(Nodes, application, stop, [mnesia]),
    ok.

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
