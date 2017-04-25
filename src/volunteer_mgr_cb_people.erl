-module(volunteer_mgr_cb_people).

-export([init/2]).

init(Req, State) ->
    {ok, ResponseBody} = volunteer_mgr_templates_people:render(),
    Reply = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], ResponseBody, Req),
    {ok, Reply, State}.
