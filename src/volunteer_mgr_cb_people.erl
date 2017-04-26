-module(volunteer_mgr_cb_people).

-export([init/2]).

init(Req, State) ->
    Method = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req),
    Reply = process_request(Method, HasBody, Req),
    {ok, Reply, State}.

process_request(<<"GET">>, _, Req) ->
    render_form_with_existing_tags(Req);
process_request(<<"POST">>, true, Req) ->
    {ok, PostVals, Req1} = cowboy_req:body_qs(Req),
    {<<"first_name">>, F} = lists:keyfind(<<"first_name">>, 1, PostVals),
    {<<"last_name">>, L} = lists:keyfind(<<"last_name">>, 1, PostVals),
    %% TODO {<<"phone">>, P} = lists:keyfind(<<"phone">>, 1, PostVals),
    P = {123, 555, 1212},
    {<<"email">>, E} = lists:keyfind(<<"email">>, 1, PostVals),
    {<<"notes">>, N} = lists:keyfind(<<"notes">>, 1, PostVals),
    Tags = [binary_to_atom(T, latin1) || {<<"tags">>, T} <- PostVals],
    ok = volmgr_db_people:create(F, L, P, E, N, Tags),
    render_form_with_existing_tags(Req1);
process_request(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Missing Body">>, Req);
process_request(_, _, Req) ->
	cowboy_req:reply(405, Req).

render_form_with_existing_tags(Req) ->
    Tags = volmgr_db_tags:retrieve(),
    Data = [{tags, [T || {T, _} <- Tags]}],
    {ok, ResponseBody} = volunteer_mgr_templates_people:render(Data),
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], ResponseBody, Req).
