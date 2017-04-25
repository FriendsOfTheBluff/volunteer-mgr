-record(volmgr_people,
        {id :: binary(),
         active :: boolean(),
         first :: binary(),
         last :: binary(),
         phone :: phone(),
         email :: binary(),
         notes :: list(binary())
        }).

-record(volmgr_people_tags,
        {volmgr_tags_id :: tag(),
         volmgr_people_id :: binary()
        }).

-record(volmgr_tags, {id :: tag(), active :: boolean()}).
