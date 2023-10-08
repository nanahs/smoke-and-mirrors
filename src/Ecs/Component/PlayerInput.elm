module Ecs.Component.PlayerInput exposing (PlayerInput(..))

import Input exposing (Input)
import Set exposing (Set)


type PlayerInput
    = PlayerInput (Set Input)
