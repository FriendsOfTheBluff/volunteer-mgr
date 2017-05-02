-module(volunteer_mgr).

-export([http_start/0]).

http_start() ->
    do_cowboy_start().

do_cowboy_start() ->
    {Ip, Port, Workers, Dispatch} = do_cowboy_configure(),
    Env = #{env => #{dispatch => Dispatch}},
    Opts = [{ip, Ip}, {port, Port}],
    cowboy:start_clear(http, Workers, Opts, Env).

do_cowboy_configure() ->
    {ok, Ip} = application:get_env(?MODULE, ip_address),
    {ok, Port} = application:get_env(?MODULE, port),
    {ok, Workers} = application:get_env(?MODULE, workers),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", volmgr_cb_default, no_state},
            {"/people", volmgr_cb_people, no_state},
            {"/tags", volmgr_cb_tags, no_state},
            {"/tag/create", volmgr_cb_tag_create, no_state},
            {"/[...]", cowboy_static, {priv_dir, ?MODULE, "static"}}
        ]}
    ]),
    {Ip, Port, Workers, Dispatch}.
