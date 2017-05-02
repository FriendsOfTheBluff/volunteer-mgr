-module(volmgr_cb_tags).

-export([init/2,
        allowed_methods/2,
        content_types_provided/2,
        to_html/2]).

-spec init(Req, Opts) -> {cowboy_rest, Req, Opts} when Req::cowboy_req:req(), Opts::any().
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

-spec allowed_methods(Req, State) -> {list(binary()), Req, State}
	when Req::cowboy_req:req(), State::any().
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

-spec content_types_provided(Req, State) ->
    {[{binary() | {binary(), binary(), '*' | [{binary(), binary()}]}, atom()}], Req, State} | {stop, Req, State}
	when Req::cowboy_req:req(), State::any().
content_types_provided(Req, State) ->
    {[
		{<<"text/html">>, to_html} %%,
		%% TODO {<<"application/json">>, to_json}
	], Req, State}.

-spec to_html(Req, State) -> {iolist(), Req, State} | {stop, Req, State}
	when Req::cowboy_req:req(), State::any().
to_html(Req, State) ->
    Tags = volmgr_db_tags:retrieve(),
    Data = [{tags, [T || {T, _} <- Tags]}],
    {ok, Body} = volmgr_cb_tags_dtl:render(Data),
    {Body, Req, State}.
