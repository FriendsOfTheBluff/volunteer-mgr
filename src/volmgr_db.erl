-module(volmgr_db).

-export([start/0]).

-spec start() ->
    ok | {timeout, list(term())} | {error, any()}.
start() ->
    mnesia:wait_for_tables(tables(), 5000).

-spec tables() -> list(atom()).
tables() ->
    [volmgr_people, volmgr_people_tags, volmgr_tags].
