module Ecs.Component exposing (Component(..))

import Ecs.Component.Dimensions exposing (Dimensions)
import Ecs.Component.Event exposing (Event)
import Ecs.Component.PlayerInput exposing (PlayerInput)
import Ecs.Component.Position exposing (Position)
import Ecs.Component.Render exposing (Render)
import Ecs.Component.Velocity exposing (Velocity)


type Component
    = Dimensions Dimensions
    | Event Event
    | PlayerInput PlayerInput
    | Position Position
    | Render Render
    | Velocity Velocity
