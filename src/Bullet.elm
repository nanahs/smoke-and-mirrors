module Bullet exposing (Bullet, init, render, toBounds, update, velocity)

import BoundingBox exposing (BoundingBox)
import Canvas
import Constants
import Vector2 exposing (Vector2)



-- Constants


speed : Float
speed =
    400


height : Float
height =
    5


width : Float
width =
    2



-- Model


type Bullet
    = Bullet Internals


type alias Internals =
    { position : Vector2
    , velocity : Vector2
    }


init : Vector2 -> Bullet
init position_ =
    Bullet
        { position = position_
        , velocity = Vector2.create { x = 0, y = 1 }
        }


velocity : Bullet -> Vector2
velocity (Bullet bullet) =
    bullet.velocity


update : Float -> Bullet -> Maybe Bullet
update delta (Bullet bullet) =
    let
        scaledVelocity =
            Vector2.scale (delta * speed) bullet.velocity

        newPos =
            Vector2.add bullet.position scaledVelocity
    in
    if Vector2.getY newPos > Constants.gameHeight then
        Nothing

    else
        Just (Bullet { bullet | position = newPos })


toBounds : Bullet -> BoundingBox
toBounds (Bullet enemey) =
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


render : Bullet -> Canvas.Renderable
render (Bullet bullet) =
    Canvas.shapes []
        [ Canvas.rect
            ( Vector2.getX bullet.position, Vector2.getY bullet.position )
            width
            height
        ]
