module Ecs.System.Shoot exposing (shoot)

import AssocSet as Set exposing (Set)
import Canvas
import Canvas.Settings as CanvasSettings
import Canvas.Settings.Advanced as CanvasSettings
import Color
import Dimensions
import Ecs.Component as Component exposing (Components)
import Ecs.Component.Dimensions exposing (Dimensions(..))
import Ecs.Component.PlayerInput exposing (PlayerInput(..))
import Ecs.Component.Position exposing (Position(..))
import Ecs.Component.Render exposing (Render(..))
import Ecs.Component.Velocity exposing (Velocity(..))
import Input exposing (Input)
import Vector2 exposing (Vector2)


type Action
    = SpawnBullet Components
    | NoOp


shoot : Set Input -> { x | components : Components } -> Action
shoot inputs entity =
    case
        ( entity.components.position
        , entity.components.playerInput
        )
    of
        ( Just (Position position), Just (PlayerInput _) ) ->
            if Set.member Input.Shoot inputs then
                Component.empty
                    |> (\c -> { c | position = Just (Position position) })
                    |> (\c -> { c | dimensions = Just (Dimensions (Dimensions.init 5 2)) })
                    |> (\c -> { c | velocity = Just (Velocity 50 (Vector2.create { x = 0, y = 1 })) })
                    |> (\c -> { c | render = Just (Render renderBullet) })
                    -- [ Component.Position position
                    -- , Component.Dimensions (Dimensions.init 5 2)
                    -- , Component.Velocity 50 (Vector2.create { x = 0, y = 1 })
                    -- , Component.Render renderBullet
                    -- ]
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
