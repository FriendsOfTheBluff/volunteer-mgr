-module(volmgr_db).

-export([tables/0,
         install/1,
         create_person/4,
         retrieve_person/1,
         create_tag/1,
         retrieve_tag/1]).

-include("volunteer_mgr.hrl").

-record(volmgr_people,
        {id :: binary(),
         first :: binary(),
         last :: binary(),
         phone :: phone(),
         email :: binary()
        }).

-record(volmgr_tags, {id :: atom()}).

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

-spec retrieve_person(Id :: binary()) -> person() | notfound.
retrieve_person(Id) ->
    F = fun() ->
        case mnesia:read({volmgr_people, Id}) of
            [#volmgr_people{id=Id, first=F, last=L, phone=P, email=E}] ->
                #person{id=Id, first=F, last=L, phone=P, email=E};
            [] ->
                notfound
        end
    end,
    mnesia:activity(transaction, F).

-spec create_tag(Tag :: atom()) -> ok | {aborted, any()}.
create_tag(notfound) ->
    {aborted, <<"invalid tag: notfound">>};
create_tag(Tag) ->
    F = fun() ->
            TagR = #volmgr_tags{id=Tag},
            mnesia:write(TagR)
        end,
    mnesia:activity(transaction, F).

-spec retrieve_tag(Tag :: atom()) -> atom() | notfound.
retrieve_tag(Tag) ->
    F = fun() ->
            TagR = #volmgr_tags{id=Tag},
            mnesia:write(TagR)
        end,
    mnesia:activity(transaction, F).
