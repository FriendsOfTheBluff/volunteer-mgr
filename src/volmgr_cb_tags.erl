-module(volmgr_cb_tags).

-export([init/2,
        allowed_methods/2,
        content_types_provided/2,
        content_types_accepted/2,
        to_html/2]).

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.
    % Method = cowboy_req:method(Req),
    % HasBody = cowboy_req:has_body(Req),
    % Reply = process_request(Method, HasBody, Req),
    % {ok, Reply, State}.

-spec allowed_methods(Req, State) -> {list(binary()), Req, State}
	when Req::cowboy_req:req(), State::any().
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

-spec content_types_provided(Req, State) ->
    {[{binary() | {binary(), binary(), '*' | [{binary(), binary()}]}, atom()}], Req, State} | {stop, Req, State}
	when Req::cowboy_req:req(), State::any().
content_types_provided(Req, State) ->
    {[
		{<<"text/html">>, to_html} %%,
		%% TODO {<<"application/json">>, to_json}
	], Req, State}.

-spec to_html(Req, State) -> {binary(), Req, State} | {stop, Req, State}
	when Req::cowboy_req:req(), State::any().
to_html(Req, State) ->
    Tags = volmgr_db_tags:retrieve(),
    Data = [{tags, [T || {T, _} <- Tags]}],
    {ok, Body} = volmgr_cb_tags_dtl:render(Data),
    {Body, Req, State}.

% -spec process_request(binary(), boolean(), cowboy_req:req()) -> cowboy_req:req().
% process_request(<<"GET">>, _, Req) ->
%     render_existing_tags(Req);
% process_request(<<"POST">>, true, Req) ->
%     {ok, PostVals, Req1} = cowboy_req:read_urlencoded_body(Req),
%     Tag = cb_util:get_post_value(<<"new_tag">>, PostVals),
%     ok = volmgr_db_tags:create(Tag),
%     render_existing_tags(Req1);
% process_request(<<"POST">>, false, Req) ->
%     Headers = #{<<"content-type">> => <<"text/plain">>},
%     cowboy_req:reply(400, Headers, <<"Missing Body">>, Req);
% process_request(_, _, Req) ->
%     cowboy_req:reply(405, Req).
% 
% -spec render_existing_tags(cowboy_req:req()) -> cowboy_req:req().
% render_existing_tags(Req) ->
%     Tags = volmgr_db_tags:retrieve(),
%     Data = [{tags, [T || {T, _} <- Tags]}],
%     {ok, ResponseBody} = volmgr_cb_tags_dtl:render(Data),
%     Headers = #{<<"content-type">> => <<"text/html">>},
%     cowboy_req:reply(200, Headers, ResponseBody, Req).
