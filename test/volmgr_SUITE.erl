-module(volmgr_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("volunteer.hrl").

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         can_save_person/1]).

all() -> [
          can_save_person
         ].

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    ok = application:set_env(mnesia, dir, Priv),
    ok = volmgr_db:install([node()]),
    ok = application:start(mnesia),
    ok = application:start(volunteer_mgr),
    Config.

end_per_suite(_Config) ->
    application:stop(mnesia).

can_save_person(_) ->
    Id = <<"abooey-bob">>,
    First = <<"Bob">>,
    Last = <<"Abooey">>,
    Phone = {345, 555, 1212},
    Email = <<"bob@abooey.com">>,
    Person = #volmgr_person{id=Id,
                            first=First,
                            last=Last,
                            phone=Phone,
                            email=Email},
    ok = volmgr_db:save(Person).
