module Ecs.System.StepPosition exposing (stepPosition)

import AssocSet as Set exposing (Set)
import Dict exposing (Dict)
import Ecs.Component as Component exposing (Component)
import Vector2


stepPosition : Float -> { x | components : Set Component } -> { x | components : Set Component }
stepPosition deltaTime entity =
    case ( Set.get Component.Position entity.components, Dict.get Component.velKey entity.components ) of
        ( Just (Component.Position position), Just (Component.Velocity speed velocity_) ) ->
            { entity
                | components =
                    Dict.update Component.positionKey
                        (\_ ->
                            Just (Component.Position (Vector2.add (Vector2.scale (deltaTime * speed) velocity_) position))
                        )
                        entity.components
            }

        _ ->
            entity
