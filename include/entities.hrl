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