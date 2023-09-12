module Enemy exposing (Enemy, init, render, toBounds)

import BoundingBox exposing (BoundingBox)
import Canvas
import Canvas.Settings as CanvasSettings
import Color
import Vector2 exposing (Vector2)



-- Constants


height : Float
height =
    5


width : Float
width =
    25


type Enemy
    = Enemy Internals


type alias Internals =
    { position : Vector2
    , velocity : Vector2
    }


init : ( Float, Float ) -> Enemy
init ( x, y ) =
    Enemy
        { position = Vector2.create { x = x, y = y }
        , velocity = Vector2.zero
        }


toBounds : Enemy -> BoundingBox
toBounds (Enemy enemey) =
    let
        ( x, y ) =
            Vector2.toTuple enemey.position
    in
    { minX = x
    , maxX = x + width
    , minY = y
    , maxY = y + height
    }



-- View


render : Enemy -> Canvas.Renderable
render (Enemy enemy) =
    Canvas.shapes [ CanvasSettings.fill Color.red ]
        [ Canvas.rect
            ( Vector2.getX enemy.position, Vector2.getY enemy.position )
            width
            height
        ]
