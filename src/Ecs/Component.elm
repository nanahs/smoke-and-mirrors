module Ecs.Component exposing (Component(..), Components, empty)

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


type alias Components =
    { dimensions : Maybe Dimensions
    , playerInput : Maybe PlayerInput
    , position : Maybe Position
    , render : Maybe Render
    , velocity : Maybe Velocity
    }


empty : Components
empty =
    { dimensions = Nothing
    , playerInput = Nothing
    , position = Nothing
    , render = Nothing
    , velocity = Nothing
    }
