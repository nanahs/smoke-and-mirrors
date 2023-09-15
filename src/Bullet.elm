module Bullet exposing (Bullet, id, init, isInBounds, render, toBounds, update, velocity)

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
    { id : String
    , position : Vector2
    , velocity : Vector2
    }


init : String -> Vector2 -> Bullet
init id_ position_ =
    Bullet
        { id = id_
        , position = position_
        , velocity = Vector2.create { x = 0, y = 1 }
        }


id : Bullet -> String
id (Bullet bullet) =
    bullet.id


velocity : Bullet -> Vector2
velocity (Bullet bullet) =
    bullet.velocity


update : Float -> Bullet -> Bullet
update delta (Bullet bullet) =
    let
        scaledVelocity =
            Vector2.scale (delta * speed) bullet.velocity

        newPos =
            Vector2.add bullet.position scaledVelocity
    in
    Bullet { bullet | position = newPos }


isInBounds : Bullet -> Bool
isInBounds (Bullet bullet) =
    Vector2.getY bullet.position <= Constants.gameHeight


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
