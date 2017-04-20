-define(PRIVDIR, code:priv_dir(volunteer_mgr)).

-type phone() :: {integer(), integer(), integer()}.

-record(person,
        {id :: binary(),
         active :: boolean(),
         first :: binary(),
         last :: binary(),
         phone :: phone(),
         email :: binary(),
         notes = [] :: list(binary())
        }).

-type person() :: #person{}.
