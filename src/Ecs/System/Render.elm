module Ecs.System.Render exposing (render)

import Canvas
import Dict exposing (Dict)
import Ecs.Component as Component exposing (Component)


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
