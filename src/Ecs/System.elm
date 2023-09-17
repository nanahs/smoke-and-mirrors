module Ecs.System exposing (Action(..), movement, render, shoot, stepPosition)

import AssocSet as Set exposing (Set)
import Canvas
import Canvas.Settings as CanvasSettings
import Canvas.Settings.Advanced as CanvasSettings
import Color
import Dict exposing (Dict)
import Dimensions exposing (Dimensions)
import Ecs.Component as Component exposing (Component)
import Input exposing (Input)
import Vector2 exposing (Vector2)


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


stepPosition : Float -> { x | components : Dict Component.Key Component } -> { x | components : Dict Component.Key Component }
stepPosition deltaTime entity =
    case ( Dict.get Component.positionKey entity.components, Dict.get Component.velKey entity.components ) of
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


render : { x | components : Dict Component.Key Component } -> Canvas.Renderable
render entity =
    case
        ( Dict.get Component.positionKey entity.components
        , Dict.get Component.renderKey entity.components
        , Dict.get Component.dimensionsKey entity.components
        )
    of
        ( Just (Component.Position position), Just (Component.Render toRender), Just (Component.Dimensions dimensions) ) ->
            toRender position dimensions

        _ ->
            Canvas.group [] []


shoot : Set Input -> { x | components : Dict Component.Key Component } -> Action
shoot inputs entity =
    case ( Dict.get Component.positionKey entity.components, Dict.get Component.shootKey entity.components ) of
        ( Just (Component.Position position), Just (Component.Shoot _) ) ->
            if Set.member Input.Shoot inputs then
                [ Component.Position position
                , Component.Dimensions (Dimensions.init 5 2)
                , Component.Velocity 50 (Vector2.create { x = 0, y = 1 })
                , Component.Render renderBullet
                ]
                    |> SpawnBullet

            else
                NoOp

        _ ->
            NoOp


type Action
    = SpawnBullet (List Component)
    | NoOp


renderBullet : Vector2 -> Dimensions -> Canvas.Renderable
renderBullet position dimensions =
    Canvas.shapes [ CanvasSettings.fill Color.darkBlue ]
        [ Canvas.rect (Vector2.toTuple position) (Dimensions.width dimensions) (Dimensions.height dimensions)
        ]
