module Mirror exposing (Mirror, init, render, toBounds)

import BoundingBox exposing (BoundingBox)
import Canvas
import Canvas.Settings as CanvasSettings
import Canvas.Settings.Advanced as CanvasSettings
import Color
import Vector2 exposing (Vector2)



-- Constants


height : Float
height =
    25


width : Float
width =
    25


type Mirror
    = Mirror Internals


type alias Internals =
    { position : Vector2
    }


init : ( Float, Float ) -> Mirror
init ( x, y ) =
    Mirror { position = Vector2.create { x = x, y = y } }


toBounds : Mirror -> BoundingBox
toBounds (Mirror mirror) =
    BoundingBox.fromPointHeightWidth (Vector2.toTuple mirror.position) height width



-- View


render : Mirror -> Canvas.Renderable
render (Mirror mirror) =
    Canvas.group []
        [ Canvas.shapes [ CanvasSettings.fill Color.gray ]
            [ Canvas.rect
                ( Vector2.getX mirror.position, Vector2.getY mirror.position )
                width
                height
            ]
        , Canvas.text
            [ CanvasSettings.transform
                [ CanvasSettings.applyMatrix { m11 = 1, m12 = 0, m21 = 0, m22 = -1, dx = 0, dy = 120 } ]
            ]
            (Vector2.toTuple mirror.position)
            "Mirror"
        ]
