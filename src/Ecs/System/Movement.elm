module Ecs.System.Movement exposing (movement)

import AssocSet exposing (Set)
import Ecs.Component exposing (Components)
import Ecs.Component.PlayerInput exposing (PlayerInput(..))
import Ecs.Component.Velocity exposing (Velocity(..))
import Input exposing (Input)


movement :
    Set Input
    -> { x | components : Components }
    -> { x | components : Components }
movement inputs entity =
    case ( entity.components.velocity, entity.components.playerInput ) of
        ( Just (Velocity speed _), Just (PlayerInput _) ) ->
            let
                comp =
                    entity.components
            in
            { entity
                | components =
                    { comp | velocity = Just (Velocity speed (Input.applyInputs inputs)) }
            }

        _ ->
            entity
