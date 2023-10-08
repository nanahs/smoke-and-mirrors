module Ecs.System.Movement exposing (movement)

import AssocSet exposing (Set)
import Dict exposing (Dict)
import Ecs.Component as Component exposing (Component)
import Input exposing (Input)


movement :
    Set Input
    -> { x | components : Dict Component.Key Component }
    -> { x | components : Dict Component.Key Component }
movement inputs entity =
    case ( Dict.get Component.velKey entity.components, Dict.get Component.movementKey entity.components ) of
        ( Just (Component.Velocity speed _), Just Component.Movement ) ->
            { entity
                | components =
                    Dict.update Component.velKey
                        (\_ ->
                            Just (Component.Velocity speed (Input.applyInputs inputs))
                        )
                        entity.components
            }

        _ ->
            entity
