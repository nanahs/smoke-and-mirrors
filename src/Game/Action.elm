port module Game.Action exposing (Action(..), isSpawnBullet)

import Vector2 exposing (Vector2)


type Action
    = SpawnBullet Vector2
    | NoOp


isSpawnBullet : Action -> Bool
isSpawnBullet action =
    case action of
        SpawnBullet _ ->
            True

        NoOp ->
            False


port requestAction : Action -> Cmd msg


port recievedAction : (Action -> msg) -> Sub msg
