module Ecs.System.Render exposing (render)

import Canvas
import Ecs.Component exposing (Components)
import Ecs.Component.Dimensions exposing (Dimensions(..))
import Ecs.Component.Position exposing (Position(..))
import Ecs.Component.Render exposing (Render(..))


render : { x | components : Components } -> Canvas.Renderable
render entity =
    case
        ( entity.components.position
        , entity.components.render
        , entity.components.dimensions
        )
    of
        ( Just (Position position), Just (Render toRender), Just (Dimensions dimensions) ) ->
            toRender position dimensions

        _ ->
            Canvas.group [] []
