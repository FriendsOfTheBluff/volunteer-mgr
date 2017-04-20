-type phone() :: {integer(), integer(), integer()}.

-record(person,
        {id :: binary(),
         active :: boolean(),
         first :: binary(),
         last :: binary(),
         phone :: phone(),
         email :: binary()
        }).

-type person() :: #person{}.
