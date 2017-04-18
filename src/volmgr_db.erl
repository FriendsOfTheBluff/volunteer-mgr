-module(volmgr_db).

-include("volunteer.hrl").

-export([save/1]).

-spec save(Person :: person()) -> ok | {aborted, any()}.
save(Person=#volmgr_person{}) ->
    mnesia:write(Person).
