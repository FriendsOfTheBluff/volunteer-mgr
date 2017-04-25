-module(volunteer_mgr_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    ok = volmgr_db:start(),
    {ok, _} = volunteer_mgr:http_start(),
    volunteer_mgr_sup:start_link().

stop(_State) ->
    ok.
