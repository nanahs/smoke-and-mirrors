module Player exposing (Player, height, init, position, render, setVelocity, toBounds, updatePosition, velocity, width)

import BoundingBox exposing (BoundingBox)
import Canvas
import Canvas.Settings as CanvasSettings
import Color
import Vector2 exposing (Vector2)



-- Constants


speed : Float
speed =
    400


height : Float
height =
    15


width : Float
width =
    15



-- Model


type Player
    = Player Internals


type alias Internals =
    { position : Vector2
    , velocity : Vector2
    }


init : ( Float, Float ) -> Player
init position_ =
    Player
        { position = Vector2.create { x = Tuple.first position_, y = Tuple.second position_ }
        , velocity = Vector2.zero
        }


position : Player -> Vector2
position (Player player) =
    player.position


velocity : Player -> Vector2
velocity (Player player) =
    player.velocity


setVelocity : Vector2 -> Player -> Player
setVelocity newVelocity (Player player) =
    Player { player | velocity = newVelocity }


updatePosition : Float -> { minX : Float, maxX : Float, minY : Float, maxY : Float } -> Player -> Player
updatePosition delta bounds (Player player) =
    let
        scaledVelocity =
            Vector2.scale (delta * speed) player.velocity

        newPos =
            Vector2.add player.position scaledVelocity
                |> Vector2.bounds bounds
    in
    Player { player | position = newPos }


toBounds : Player -> BoundingBox
toBounds (Player player) =
    let
        ( x, y ) =
            Vector2.toTuple player.position
    in
    { minX = x
    , maxX = x + width
    , minY = y
    , maxY = y + height
    }



-- VIEW


render : Player -> Canvas.Renderable
render (Player player) =
    Canvas.shapes [ CanvasSettings.fill Color.darkBlue ]
        [ Canvas.rect
            ( Vector2.getX player.position, Vector2.getY player.position )
            width
            height
        ]
