-module(volmgr_SUITE).

-include("types.hrl").
-include("entities.hrl").

-include_lib("common_test/include/ct.hrl").

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         create_and_retrieve_person_by_id/1,
         create_person_with_unknown_tag/1,
         retrieve_all_people/1,
         retrieve_people_by_tag/1,
         create_and_retrieve_tag_by_id/1,
         creating_tag_with_reserved_word_errors/1,
         retrieve_all_tags/1,
         ensure_tags_returns_notfound/1
        ]).

all() -> [
          create_and_retrieve_person_by_id,
          create_person_with_unknown_tag,
          retrieve_all_people,
          retrieve_people_by_tag,
          create_and_retrieve_tag_by_id,
          creating_tag_with_reserved_word_errors,
          retrieve_all_tags,
          ensure_tags_returns_notfound
         ].

init_per_suite(Config) ->
    ok = mnesia:start(),
    ok = volmgr_db_schema:init_tables(),
    {ok, _Apps} = application:ensure_all_started(volunteer_mgr),
    Config.

end_per_suite(Config) ->
    Config.

create_and_retrieve_person_by_id(_) ->
    First = <<"First1">>,
    Last = <<"Last1">>,
    Phone = {345, 555, 1212},
    Email = <<"first1@last1.com">>,
    Notes = [<<"Note 1">>, <<"Note 2">>],
    ok = volmgr_db_people:create(First, Last, Phone, Email, Notes),
    {error, notfound} = volmgr_db_people:retrieve(<<"does-not-exist">>),
    Id = <<"Last1-First1">>,
    #person{id=Id, first=First, last=Last,
            phone=Phone, email=Email, notes=Notes} = volmgr_db_people:retrieve(Id).

create_person_with_unknown_tag(_) ->
    First = <<"First2">>,
    Last = <<"Last2">>,
    Phone = {345, 555, 1212},
    Email = <<"first2@last2.com">>,
    Notes = [<<"Note 1">>, <<"Note 2">>],
    Tags = [unknown1, unknown2],
    {error, notfound} = volmgr_db_people:create(First, Last, Phone, Email, Notes, Tags).

retrieve_all_people(_) ->
    First = <<"Frank">>,
    Last = <<"Barker">>,
    Phone = {456, 555, 1212},
    Email = <<"frankg@gmail.com">>,
    ok = volmgr_db_people:create(First, Last, Phone, Email, []),
    WantId = <<"Barker-Frank">>,
    Want = #person{id=WantId, active=true,
                   first=First, last=Last,
                   phone=Phone, email=Email},
    Got = volmgr_db_people:retrieve(),
    Pred = fun(Item) ->
               Item =:= Want
           end,
    true = lists:any(Pred, Got).

retrieve_people_by_tag(_) ->
    Tag = retrieve_people_by_tag,
    ok = volmgr_db:create_tag(Tag),
    F1 = <<"F1">>,
    L1 = <<"L1">>,
    P1 = {1, 555, 1212},
    E1 = <<"f1@gmail.com">>,
    Want1 = #person{id = <<"L1-F1">>, active=true,
                    first=F1, last=L1,
                    phone=P1, email=E1},
    ok = volmgr_db_people:create(F1, L1, P1, E1, [], [retrieve_people_by_tag]),
    F2 = <<"F2">>,
    L2 = <<"L2">>,
    P2 = {2, 555, 1212},
    E2 = <<"f2@gmail.com">>,
    Want2 = #person{id = <<"L2-F2">>, active=true,
                    first=F2, last=L2,
                    phone=P2, email=E2},
    ok = volmgr_db_people:create(F2, L2, P2, E2, [], [retrieve_people_by_tag]),
    Got = volmgr_db_people:retrieve_by_tag(retrieve_people_by_tag),
    true = lists:member(Want1, Got),
    true = lists:member(Want2, Got).

create_and_retrieve_tag_by_id(_) ->
    Tag = foo,
    ok = volmgr_db:create_tag(Tag),
    {error, notfound} = volmgr_db:retrieve_tag('unknown-tag-should-not-be-saved'),
    {Tag, true} = volmgr_db:retrieve_tag(Tag).

creating_tag_with_reserved_word_errors(_) ->
    % The following aren't reserved, but we don't want them as tags since they have meaning
    Bad = ['ok', 'error', 'notfound', 'undefined'],
    % http://erlang.org/doc/reference_manual/introduction.html
    Res = ['after', 'and', 'andalso',
           'band', 'begin', 'bnot', 'bor', 'bsl', 'bsr', 'bxor',
           'case', 'catch', 'cond',
           'div', 'end', 'fun', 'if', 'let', 'not',
           'of', 'or', 'orelse',
           'receive', 'rem', 'try', 'when', 'xor'],
    Pred = fun(R) -> {error, invalid_tag} =:= volmgr_db:create_tag(R) end,
    true = lists:all(Pred, Bad ++ Res).

retrieve_all_tags(_) ->
    Tags = [foo, bar, baz, bat, frazzle],
    Want = [{T, true} || T <- Tags],
    ok = volmgr_db:create_tags(Tags),
    Got = volmgr_db:retrieve_tags(),
    Pred = fun(W) ->
               true =:= lists:member(W, Got)
           end,
    true = lists:all(Pred, Want).

ensure_tags_returns_notfound(_) ->
    Tags = [foo, bar, you_aint_gonna_find_this],
    {error, notfound} = volmgr_db:ensure_tags(Tags).
