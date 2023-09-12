module Mirror exposing (Mirror, init, render, toBounds)

import BoundingBox exposing (BoundingBox)
import Canvas
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


init : Mirror
init =
    Mirror { position = Vector2.create { x = 50, y = 50 } }


toBounds : Mirror -> BoundingBox
toBounds (Mirror mirror) =
    BoundingBox.fromPointHeightWidth (Vector2.toTuple mirror.position) height width



-- View


render : Mirror -> Canvas.Shape
render (Mirror mirror) =
    Canvas.rect
        ( Vector2.getX mirror.position, Vector2.getY mirror.position )
        width
        height


drawOffset : ( Float, Float ) -> Float -> Float -> ( Float, Float )
drawOffset position_ width_ height_ =
    position_
        |> Tuple.mapBoth (\x -> x - (width_ / 2)) (\y -> y - (height_ / 2))
