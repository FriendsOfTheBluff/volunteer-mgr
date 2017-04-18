-type phone() :: {integer(), integer(), integer()}.

-record(volmgr_person,
        {id :: binary(),
         first :: binary(),
         last :: binary(),
         phone :: phone(),
         email :: binary()
        }).

-type person() :: #volmgr_person{}.
