-module(volunteer_mgr_cb_tags).

-export([init/2]).

-spec init(cowboy_req:req(), term()) -> {ok, any(), any()}.
init(Req, State) ->
    Method = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req),
    Reply = process_request(Method, HasBody, Req),
    {ok, Reply, State}.

-spec process_request(binary(), boolean(), cowboy_req:req()) -> {ok, any(), any()}.
process_request(<<"GET">>, _, Req) ->
    render_existing_tags(Req);
process_request(<<"POST">>, true, Req) ->
    {ok, PostVals, Req1} = cowboy_req:body_qs(Req),
    TagBin = proplists:get_value(<<"new_tag">>, PostVals),
    Tag = binary_to_atom(TagBin, latin1),
    ok = volmgr_db_tags:create(Tag),
    render_existing_tags(Req1);
process_request(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Missing Body">>, Req);
process_request(_, _, Req) ->
	cowboy_req:reply(405, Req).

render_existing_tags(Req) ->
    Tags = volmgr_db_tags:retrieve(),
    Data = [{tags, [T || {T, _} <- Tags]}],
    {ok, ResponseBody} = volunteer_mgr_templates_tags:render(Data),
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], ResponseBody, Req).
