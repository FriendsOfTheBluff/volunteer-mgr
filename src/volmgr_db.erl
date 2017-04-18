-module(volmgr_db).

-include("volunteer.hrl").

-export([install/1,
         create_person/4]).

-spec install(list(node())) -> ok | {error, any()}.
install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    % TODO multicall rval
    rpc:multicall(Nodes, application, start, [mnesia]),
    VolmgrPersonOpts = [
               {attributes, record_info(fields, volmgr_person)},
               {disc_copies, Nodes},
               {type, ordered_set}
              ],
    mnesia:create_table(volmgr_person, VolmgrPersonOpts),
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
            Person = #volmgr_person{id=Id,
                                    first=First,
                                    last=Last,
                                    phone=Phone,
                                    email=Email},
            mnesia:write(Person)
        end,
    mnesia:activity(transaction, F).
