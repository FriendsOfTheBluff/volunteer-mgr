-module(cb_util).

-export([get_post_value/2,
         get_phone/1]).

-include("types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec get_post_value(binary(), list({binary(), binary() | true})) -> binary().
get_post_value(Key, Vals) ->
    case lists:keyfind(Key, 1, Vals) of
        false -> <<"undefined">>;
        V -> V
    end.

-spec get_phone(list({binary(), binary() | true})) ->
    {ok, phone()} | {error, term()}.
get_phone(Vals) ->
    PhoneBin = case lists:keyfind(<<"phone">>, 1, Vals) of
                   false -> <<"5095551212">>;
                   P -> P
               end,
    parse_phone(PhoneBin).

-spec parse_phone(binary()) -> {ok, phone()} | {error, term()}.
parse_phone(_PhoneBin) ->
    {error, not_implemented}.

-ifdef(TEST).
parse_phone_test_() ->
    [?_assert(parse_phone(<<"5095551212">>) =:= {509, 555, 1212}),
     ?_assert(parse_phone(<<"509 555 1212">>) =:= {509, 555, 1212}),
     ?_assert(parse_phone(<<"509-555-1212">>) =:= {509, 555, 1212}),
     ?_assert(parse_phone(<<"509555-1212">>) =:= {509, 555, 1212}),
     ?_assert(parse_phone(<<"XX509Y555ZZ1212">>) =:= {509, 555, 1212})
    ].
-endif.
