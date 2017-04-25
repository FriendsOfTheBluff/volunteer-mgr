-module(volmgr_db_schema).

-export([init_tables/0,
         init_tables/1,
         init_schema/0]).

-include("types.hrl").
-include("db.hrl").

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
    ok = init_tables(disc_copies);
handle_create_schema(Err) ->
    io:format(standard_error, "Create schema unexpected result: ~p~n", [Err]).
