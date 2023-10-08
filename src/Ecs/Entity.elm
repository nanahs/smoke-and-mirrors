module Ecs.Entity exposing (create)


create : { x | nextEntityId : Int } -> ( Int, { x | nextEntityId : Int } )
create world =
    ( world.nextEntityId, { world | nextEntityId = world.nextEntityId + 1 } )
