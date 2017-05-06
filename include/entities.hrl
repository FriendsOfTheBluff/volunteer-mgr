-type phone() :: {integer(), integer(), integer()}.
-type tag() :: binary().
-type person_id() :: binary().

-record(person,
        {id :: person_id(),
         active :: boolean(),
         first :: binary(),
         last :: binary(),
         phone :: phone(),
         email :: binary(),
         notes = [] :: list(binary())
        }).

-type person() :: #person{}.
