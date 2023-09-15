module Player exposing (Player, height, init, position, render, toBounds, update, width)

import AssocSet as Set exposing (Set)
import BoundingBox exposing (BoundingBox)
import Canvas
import Canvas.Settings as CanvasSettings
import Color
import Constants
import Game.Action as Action exposing (Action)
import Input exposing (Input)
import Vector2 exposing (Vector2)



-- Constants


speed : Float
speed =
    350


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
    , shootTimer : Float
    }


init : ( Float, Float ) -> Player
init position_ =
    Player
        { position = Vector2.create { x = Tuple.first position_, y = Tuple.second position_ }
        , velocity = Vector2.zero
        , shootTimer = 0
        }


position : Player -> Vector2
position (Player player) =
    player.position


update : Float -> Set Input -> Player -> ( Player, Action )
update delta inputs (Player player) =
    let
        velocity =
            Input.applyInputs inputs
                |> Vector2.scale (delta * speed)

        newPos =
            Vector2.add player.position velocity
                |> Vector2.bounds bounds

        didShoot =
            Set.member Input.Shoot inputs && canShoot player
    in
    ( Player
        { player
            | position = newPos
            , shootTimer =
                if not (canShoot player) then
                    player.shootTimer - delta

                else if didShoot then
                    Constants.shootDelay

                else
                    player.shootTimer
        }
    , if didShoot then
        Action.SpawnBullet player.position

      else
        Action.NoOp
    )


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


bounds : { minX : Float, maxX : Float, minY : Float, maxY : Float }
bounds =
    Constants.gameBounds width


canShoot : Internals -> Bool
canShoot model =
    model.shootTimer <= 0



-- VIEW


render : Player -> Canvas.Renderable
render (Player player) =
    Canvas.shapes [ CanvasSettings.fill Color.darkBlue ]
        [ Canvas.rect
            ( Vector2.getX player.position, Vector2.getY player.position )
            width
            height
        ]
