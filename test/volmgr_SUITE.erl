-module(volmgr_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("volunteer.hrl").

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         can_create_person/1]).

all() -> [can_create_person].

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    ok = application:set_env(mnesia, dir, Priv),
    ok = volmgr_db:install([node()]),
    {ok, _Apps} = application:ensure_all_started(volunteer_mgr),
    Config.

end_per_suite(_Config) ->
    application:stop(mnesia).

can_create_person(_) ->
    % Id = <<"Abooey-Bob">>,
    First = <<"Bob">>,
    Last = <<"Abooey">>,
    Phone = {345, 555, 1212},
    Email = <<"bob@abooey.com">>,
    ok = volmgr_db:create_person(First, Last, Phone, Email).
