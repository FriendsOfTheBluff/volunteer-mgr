-module(volmgr_cb_people).

-export([init/2]).

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req, State) ->
    Method = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),
    Reply = process_request(Method, HasBody, Req),
    {ok, Reply, State}.

-spec process_request(binary(), boolean(), cowboy_req:req()) -> cowboy_req:req().
process_request(<<"GET">>, _, Req) ->
    render_form_with_existing_tags(Req);
process_request(<<"POST">>, true, Req) ->
    {ok, PostVals, Req1} = cowboy_req:read_urlencoded_body(Req),
    F = cb_util:get_post_value(<<"first_name">>, PostVals),
    L = cb_util:get_post_value(<<"last_name">>, PostVals),
    P = cb_util:get_phone(PostVals),
    E = cb_util:get_post_value(<<"email">>, PostVals),
    N = cb_util:get_post_value(<<"notes">>, PostVals),
    Tags = [T || {<<"tags">>, T} <- PostVals],
    ok = volmgr_db_people:create(F, L, P, E, [N], Tags),
    render_form_with_existing_tags(Req1);
process_request(<<"POST">>, false, Req) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    cowboy_req:reply(400, Headers, <<"Missing Body">>, Req);
process_request(_, _, Req) ->
    cowboy_req:reply(405, Req).

-spec render_form_with_existing_tags(cowboy_req:req()) -> cowboy_req:req().
render_form_with_existing_tags(Req) ->
    Tags = volmgr_db_tags:retrieve(),
    Data = [{tags, [T || {T, _} <- Tags]}],
    {ok, ResponseBody} = volmgr_cb_people_dtl:render(Data),
    Headers = #{<<"content-type">> => <<"text/html">>},
    cowboy_req:reply(200, Headers, ResponseBody, Req).
