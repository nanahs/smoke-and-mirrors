module Clone exposing (Clone, init, render, toBounds, update)

import AssocSet as Set exposing (Set)
import BoundingBox exposing (BoundingBox)
import Bullet exposing (Bullet)
import Canvas
import Canvas.Settings as CanvasSettings
import Color
import Constants
import Input exposing (Input)
import Queue exposing (Queue)
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


type Clone
    = Clone Internals


type alias Internals =
    { position : Vector2
    , velocity : Vector2
    , inputs : Queue (Set Input)
    , shootTimer : Float
    }


init : ( Float, Float ) -> Int -> Clone
init position_ cloneNumber =
    Clone
        { position = Vector2.create { x = Tuple.first position_, y = Tuple.second position_ }
        , velocity = Vector2.zero
        , inputs = Queue.fromListFIFO (List.repeat (20 * cloneNumber) Set.empty)
        , shootTimer = 0
        }


update : Float -> Set Input -> Clone -> ( Clone, Maybe Bullet )
update delta newInputs (Clone clone) =
    let
        maybeInput =
            clone.inputs
                |> Queue.head

        maybevelocity =
            maybeInput
                |> Maybe.map Input.applyInputs

        scaledVelocity =
            case maybevelocity of
                Just vel ->
                    Vector2.scale (delta * speed) vel

                Nothing ->
                    Vector2.zero

        newPos =
            Vector2.add clone.position scaledVelocity
                |> Vector2.bounds bounds

        triedToShoot =
            maybeInput
                |> Maybe.map (Set.member Input.Shoot)
                |> Maybe.withDefault False

        didShoot =
            triedToShoot && canShoot clone
    in
    ( Clone
        { clone
            | position = newPos
            , inputs =
                clone.inputs
                    |> Queue.dequeue
                    |> Queue.enqueue newInputs
            , shootTimer =
                if not (canShoot clone) then
                    clone.shootTimer - delta

                else if didShoot then
                    Constants.shootDelay

                else
                    clone.shootTimer
        }
    , if didShoot then
        Just (Bullet.init clone.position)

      else
        Nothing
    )


toBounds : Clone -> BoundingBox
toBounds (Clone clone) =
    let
        ( x, y ) =
            Vector2.toTuple clone.position
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


render : Clone -> Canvas.Renderable
render (Clone clone) =
    Canvas.shapes [ CanvasSettings.fill Color.lightBlue ]
        [ Canvas.rect
            ( Vector2.getX clone.position, Vector2.getY clone.position )
            width
            height
        ]
