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

-spec get_phone(list({binary(), binary() | true})) -> phone().
get_phone(Vals) ->
    PhoneBin = case lists:keyfind(<<"phone">>, 1, Vals) of
                   false -> <<"5095551212">>;
                   P -> P
               end,
    parse_phone(PhoneBin).

-spec parse_phone(binary()) -> phone().
parse_phone(PhoneBin) ->
    Digits = [D || D <- binary_to_list(PhoneBin), D >= $0 andalso D =< $9],
    parse_digits(Digits).

-spec parse_digits(list(integer())) -> phone().
parse_digits(Digits) when is_list(Digits), length(Digits) =/= 10 ->
    {999, 999, 9999};
parse_digits(Digits) when is_list(Digits), length(Digits) =:= 10 ->
    Area = list_to_integer(lists:sublist(Digits, 1, 3)),
    P1 = list_to_integer(lists:sublist(Digits, 4, 3)),
    P2 = list_to_integer(lists:sublist(Digits, 7, 4)),
    {Area, P1, P2}.

-ifdef(TEST).
parse_phone_test_() ->
    [?_assertMatch({509,555,1212}, parse_phone(<<"5095551212">>)),
     ?_assertMatch({509,555,1212}, parse_phone(<<"509 555 1212">>)),
     ?_assertMatch({509,555,1212}, parse_phone(<<"509-555-1212">>)),
     ?_assertMatch({509,555,1212}, parse_phone(<<"509555-1212">>)),
     ?_assertMatch({509,555,1212}, parse_phone(<<"XX509Y555ZZ1212">>)),
     ?_assertMatch({999,999,9999}, parse_phone(<<"XX509Y5ZZ1212">>))
    ].
-endif.
