-module(volmgr_cb_default).

-export([init/2]).

init(Req, State) ->
    {ok, ResponseBody} = volunteer_mgr_templates_index:render(),
    Headers = #{<<"content-type">> => <<"text/html">>},
    Reply = cowboy_req:reply(200, Headers, ResponseBody, Req),
    {ok, Reply, State}.
