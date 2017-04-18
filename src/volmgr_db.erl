-module(volmgr_db).

-include("volunteer.hrl").

-export([save/1]).

save(_Person=#volmgr_person{}) ->
    error.
