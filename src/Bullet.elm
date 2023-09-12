module Bullet exposing (..)

import Canvas
import Vector2 exposing (Vector2)



-- Constants


speed : Float
speed =
    500


height : Float
height =
    10


width : Float
width =
    5



-- Model


type Bullet
    = Bullet Internals


type alias Internals =
    { position : Vector2
    , velocity : Vector2
    }


init : ( Float, Float ) -> Bullet
init position_ =
    Bullet
        { position = Vector2.create { x = Tuple.first position_, y = Tuple.second position_ }
        , velocity = Vector2.create { x = 0, y = 1 }
        }


position : Bullet -> Vector2
position (Bullet bullet) =
    bullet.position


velocity : Bullet -> Vector2
velocity (Bullet bullet) =
    bullet.velocity


updatePosition : Float -> Bullet -> Bullet
updatePosition delta (Bullet bullet) =
    let
        scaledVelocity =
            Vector2.scale (delta * speed) bullet.velocity

        newPos =
            Vector2.add bullet.position scaledVelocity
    in
    -- if y value > canvas then dont return a bullet
    -- OPTIMIZE BY POOLING THE BULLETS
    Bullet { bullet | position = newPos }



-- View


render : Bullet -> Canvas.Shape
render (Bullet bullet) =
    Canvas.rect
        (drawOffset ( Vector2.getX bullet.position, Vector2.getY bullet.position ) width height)
        width
        height


drawOffset : ( Float, Float ) -> Float -> Float -> ( Float, Float )
drawOffset position_ width_ height_ =
    position_
        |> Tuple.mapBoth (\x -> x - (width_ / 2)) (\y -> y - (height_ / 2))
