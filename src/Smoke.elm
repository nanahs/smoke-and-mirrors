module Smoke exposing (Smoke, init, render)

import Canvas
import Canvas.Settings as CanvasSettings
import Color
import Vector2 exposing (Vector2)


type Smoke
    = Smoke Internals


type alias Internals =
    { position : Vector2
    , width : Float
    , height : Float
    }


init : Vector2 -> Float -> Float -> Smoke
init position width height =
    Smoke { position = position, width = width, height = height }


render : Smoke -> Canvas.Renderable
render (Smoke smoke) =
    Canvas.shapes [ CanvasSettings.fill Color.darkGray ]
        [ Canvas.rect
            ( Vector2.getX smoke.position, Vector2.getY smoke.position )
            smoke.width
            smoke.height
        ]
