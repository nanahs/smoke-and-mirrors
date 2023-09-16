module Ecs.Component exposing (Component(..), Key, renderKey, shootKey, toKey, transformKey, velKey)

import Canvas
import Transform exposing (Transform)
import Vector2 exposing (Vector2)


type Component
    = Transform Transform
    | Velocity Float Vector2
    | Render (Transform -> Canvas.Renderable)
    | Shoot Bool


type alias Key =
    Int


toKey : Component -> Key
toKey component =
    case component of
        Transform _ ->
            1

        Velocity _ _ ->
            2

        Render _ ->
            3

        Shoot _ ->
            4


transformKey : Int
transformKey =
    1


velKey : Int
velKey =
    2


renderKey : Int
renderKey =
    3


shootKey : Int
shootKey =
    4
