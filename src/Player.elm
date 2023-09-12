module Player exposing (Player, init, position, render, render_, setVelocity, updatePosition, velocity)

import Canvas
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



-- VIEW


render : Player -> Canvas.Shape
render (Player player) =
    Canvas.rect
        (drawOffset ( Vector2.getX player.position, Vector2.getY player.position ) width height)
        width
        height


render_ : Player -> Canvas.Shape
render_ (Player player) =
    Canvas.path (Vector2.toTuple player.position)
        [ player.position
            |> Vector2.add (Vector2.create { x = width, y = -height })
            |> Vector2.toTuple
            |> Canvas.lineTo
        , player.position
            |> Vector2.add (Vector2.create { x = -width, y = -height })
            |> Vector2.toTuple
            |> Canvas.lineTo
        ]


drawOffset : ( Float, Float ) -> Float -> Float -> ( Float, Float )
drawOffset position_ width_ height_ =
    position_
        |> Tuple.mapBoth (\x -> x - (width_ / 2)) (\y -> y - (height_ / 2))
