-type db_phone() :: {integer(), integer(), integer()}.
-type db_tag() :: binary().
-type db_person_id() :: binary().

-record(volmgr_people,
        {id :: db_person_id(),
         active :: boolean(),
         first :: binary(),
         last :: binary(),
         phone :: db_phone(),
         email :: binary(),
         notes = [] :: list(binary()),
         tags = [] :: list(db_tag())
        }).

-record(volmgr_people_tags,
        {volmgr_tags_id :: db_tag(),
         volmgr_people_id :: db_person_id()
        }).

-record(volmgr_tags, {id :: db_tag(), active :: boolean()}).
