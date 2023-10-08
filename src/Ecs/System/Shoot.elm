module Ecs.System.Shoot exposing (shoot)

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


type Action
    = SpawnBullet (List Component)
    | NoOp


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


renderBullet : Vector2 -> Dimensions -> Canvas.Renderable
renderBullet position dimensions =
    Canvas.shapes [ CanvasSettings.fill Color.darkBlue ]
        [ Canvas.rect (Vector2.toTuple position) (Dimensions.width dimensions) (Dimensions.height dimensions)
        ]
