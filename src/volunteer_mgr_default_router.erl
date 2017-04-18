-module(volunteer_mgr_default_router).
-export([init/2]).

init(Req, Page) ->
    {ok, ResponseBody} = volunteer_mgr_templates_index:render(),
    Reply = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], ResponseBody, Req),
    {ok, Reply, Page}.
