-module(volmgr_cb_people).

-export([init/2,
        allowed_methods/2,
        content_types_provided/2,
        to_html/2]).

-include("entities.hrl").

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
    People = [to_proplist(P) || P <- volmgr_db_people:retrieve()],
    {ok, Body} = volmgr_cb_people_dtl:render([{people, People}]),
    {Body, Req, State}.

to_proplist(#person{first=F, last=L, email=E, phone=P, notes=N}) ->
    [{first, F}, {last, L}, {email, E}, {phone, to_phone(P)}, {notes, N}].

to_phone({N0, N1, N2}) ->
    B0 = integer_to_binary(N0),
    B1 = integer_to_binary(N1),
    B2 = integer_to_binary(N2),
    erlang:iolist_to_binary([B0, $-, B1, $-, B2]).
