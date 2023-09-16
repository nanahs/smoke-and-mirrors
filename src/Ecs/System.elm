module Ecs.System exposing (playerVelocity, render, shoot, transform)

import AssocSet exposing (Set)
import Canvas
import Dict exposing (Dict)
import Ecs.Component as Component exposing (Component)
import Input exposing (Input)
import Transform
import Vector2


playerVelocity :
    Float
    -> Set Input
    -> { x | components : Dict Component.Key Component }
    -> { x | components : Dict Component.Key Component }
playerVelocity deltaTime inputs entity =
    case Dict.get Component.velKey entity.components of
        Just (Component.Velocity speed _) ->
            let
                newVelocity =
                    Input.applyInputs inputs
                        |> Vector2.scale (deltaTime * speed)
            in
            { entity
                | components =
                    Dict.update Component.velKey
                        (\_ ->
                            Just (Component.Velocity speed newVelocity)
                        )
                        entity.components
            }

        _ ->
            entity


transform : { x | components : Dict Component.Key Component } -> { x | components : Dict Component.Key Component }
transform entity =
    case ( Dict.get Component.transformKey entity.components, Dict.get Component.velKey entity.components ) of
        ( Just (Component.Transform transform_), Just (Component.Velocity _ velocity) ) ->
            { entity
                | components =
                    Dict.update Component.transformKey
                        (\_ ->
                            Just (Component.Transform (Transform.updatePosition (Vector2.add velocity) transform_))
                        )
                        entity.components
            }

        _ ->
            entity


render : { x | components : Dict Component.Key Component } -> Canvas.Renderable
render entity =
    case ( Dict.get Component.transformKey entity.components, Dict.get Component.renderKey entity.components ) of
        ( Just (Component.Transform transform_), Just (Component.Render toRender) ) ->
            toRender transform_

        _ ->
            Canvas.group [] []


shoot : { x | components : Dict Component.Key Component } -> ()
shoot entity =
    case ( Dict.get Component.transformKey entity.components, Dict.get Component.renderKey entity.components ) of
        ( Just (Component.Transform transform_), Just (Component.Shoot tryShoot) ) ->
            ()

        _ ->
            ()
