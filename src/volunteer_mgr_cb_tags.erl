-module(volunteer_mgr_cb_tags).

-export([init/2]).

init(Req, State) ->
    Data = [{tags, [foo, bar, baz, bat]}],
    {ok, ResponseBody} = volunteer_mgr_templates_tags:render(Data),
    Reply = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], ResponseBody, Req),
    {ok, Reply, State}.
