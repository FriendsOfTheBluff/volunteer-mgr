-module(volmgr_db).

-export([install/1,
         create_person/4,
         retrieve_person/1]).

-include("volunteer_mgr.hrl").

-record(volmgr_people,
        {id :: binary(),
         first :: binary(),
         last :: binary(),
         phone :: phone(),
         email :: binary()
        }).

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
    mnesia:create_table(volmgr_people, VolmgrPeopleOpts),
    % TODO multicall rval
    rpc:multicall(Nodes, application, stop, [mnesia]),
    ok.

-spec create_person(First :: binary(),
                    Last :: binary(),
                    Phone :: {integer(), integer(), integer()},
                    Email :: binary()) -> ok | {aborted, any()}.
create_person(First, Last, Phone, Email) ->
    Id = erlang:iolist_to_binary([Last, $-, First]),
    F = fun() ->
            Person = #volmgr_people{id=Id,
                                    first=First,
                                    last=Last,
                                    phone=Phone,
                                    email=Email},
            mnesia:write(Person)
        end,
    mnesia:activity(transaction, F).

-spec retrieve_person(Id :: binary()) -> person().
retrieve_person(Id) ->
    F = fun() ->
        case mnesia:read({volmgr_people, Id}) of
            [#volmgr_people{id=Id, first=F, last=L, phone=P, email=E}] ->
                #person{id=Id, first=F, last=L, phone=P, email=E};
            [] ->
                not_found
        end
    end,
    mnesia:activity(transaction, F).
