module Ecs.Component.Event exposing (Event(..), noop)

import Vector2 exposing (Vector2)


type Event
    = SpawnBullet Vector2
    | RemoveEntity Int
    | NoOp


noop : Event
noop =
    NoOp
