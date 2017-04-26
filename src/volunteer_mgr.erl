-module(volunteer_mgr).

-export([http_start/0]).

http_start() ->
    do_cowboy_start().

do_cowboy_start() ->
    {Ip, Port, Workers, Dispatch} = do_cowboy_configure(),
    cowboy:start_http(http, Workers,
        [{ip, Ip}, {port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ).

do_cowboy_configure() ->
    {ok, Ip} = application:get_env(?MODULE, ip_address),
    {ok, Port} = application:get_env(?MODULE, port),
    {ok, Workers} = application:get_env(?MODULE, workers),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", volunteer_mgr_cb_default, no_state},
            {"/people", volmgr_cb_people, no_state},
            {"/tags", volmgr_cb_tags, no_state},
            {"/static/[...]", cowboy_static, {priv_dir, ?MODULE, "static"}},
            {"/[...]", cowboy_static, {priv_dir, ?MODULE, "pages"}}
        ]}
    ]),
    {Ip, Port, Workers, Dispatch}.
