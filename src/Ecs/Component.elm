module Ecs.Component exposing
    ( Component(..)
    , Key
    , dimensionsKey
    , movementKey
    , positionKey
    , renderKey
    , shootKey
    , toKey
    , velKey
    )

import Canvas
import Dimensions exposing (Dimensions)
import Vector2 exposing (Vector2)


type Component
    = Position Vector2
    | Velocity Float Vector2
    | Render (Vector2 -> Dimensions -> Canvas.Renderable)
    | Movement
    | Shoot Bool
    | Dimensions Dimensions


type alias Key =
    Int


toKey : Component -> Key
toKey component =
    case component of
        Position _ ->
            1

        Velocity _ _ ->
            2

        Render _ ->
            3

        Shoot _ ->
            4

        Movement ->
            5

        Dimensions _ ->
            6


positionKey : Int
positionKey =
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


movementKey : Int
movementKey =
    5


dimensionsKey : Int
dimensionsKey =
    6
