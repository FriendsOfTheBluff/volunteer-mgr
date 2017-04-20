-module(volunteer_mgr_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    ok = mnesia:wait_for_tables(volmgr_db:tables(), 5000),
    {ok, _} = volunteer_mgr:http_start(),
    volunteer_mgr_sup:start_link().

stop(_State) ->
    ok.
