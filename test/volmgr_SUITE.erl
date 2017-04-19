-module(volmgr_SUITE).

-include("volunteer_mgr.hrl").

-include_lib("common_test/include/ct.hrl").

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         create_and_retrieve_person_by_id/1]).

all() -> [
          create_and_retrieve_person_by_id
         ].

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    ok = application:set_env(mnesia, dir, Priv),
    ok = volmgr_db:install([node()]),
    {ok, _Apps} = application:ensure_all_started(volunteer_mgr),
    Config.

end_per_suite(_Config) ->
    application:stop(mnesia).

create_and_retrieve_person_by_id(_) ->
    First = <<"Bob">>,
    Last = <<"Abooey">>,
    Phone = {345, 555, 1212},
    Email = <<"bob@abooey.com">>,
    ok = volmgr_db:create_person(First, Last, Phone, Email),
    not_found = volmgr_db:retrieve_person(<<"does-not-exist">>),
    Id = <<"Abooey-Bob">>,
    #person{id=Id, first=First, last=Last,
            phone=Phone, email=Email} = volmgr_db:retrieve_person(Id).
