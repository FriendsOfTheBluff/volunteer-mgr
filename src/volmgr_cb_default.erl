-module(volmgr_cb_default).

-export([init/2]).

init(Req, State) ->
    {ok, ResponseBody} = volmgr_cb_index_dtl:render(),
    Headers = #{<<"content-type">> => <<"text/html">>},
    Reply = cowboy_req:reply(200, Headers, ResponseBody, Req),
    {ok, Reply, State}.
