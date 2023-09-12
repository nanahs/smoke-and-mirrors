module Clone exposing (Clone, init, position, render, setVelocity, toBounds, update, velocity)

import AssocSet as Set exposing (Set)
import BoundingBox exposing (BoundingBox)
import Canvas
import Canvas.Settings as CanvasSettings
import Color
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
    }


init : ( Float, Float ) -> Clone
init position_ =
    Clone
        { position = Vector2.create { x = Tuple.first position_, y = Tuple.second position_ }
        , velocity = Vector2.zero
        , inputs = Queue.fromListFIFO (List.repeat 30 Set.empty)
        }


position : Clone -> Vector2
position (Clone clone) =
    clone.position


velocity : Clone -> Vector2
velocity (Clone clone) =
    clone.velocity


setVelocity : Vector2 -> Clone -> Clone
setVelocity newVelocity (Clone clone) =
    Clone { clone | velocity = newVelocity }


update : Float -> Set Input -> Clone -> Clone
update delta newInputs (Clone clone) =
    let
        maybevelocity =
            clone.inputs
                |> Queue.head
                |> Maybe.map
                    (\inputs ->
                        inputs
                            |> Set.toList
                            |> List.foldl applyMovementInput Vector2.zero
                    )

        scaledVelocity =
            case maybevelocity of
                Just vel ->
                    Vector2.scale (delta * speed) vel

                Nothing ->
                    Vector2.zero

        newPos =
            Vector2.add clone.position scaledVelocity
    in
    Clone
        { clone
            | position = newPos
            , inputs =
                clone.inputs
                    |> Queue.dequeue
                    |> Queue.enqueue newInputs
        }


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


applyMovementInput : Input -> Vector2 -> Vector2
applyMovementInput input accPos =
    Vector2.add (Input.toVector2 input) accPos



-- VIEW


render : Clone -> Canvas.Renderable
render (Clone clone) =
    Canvas.shapes [ CanvasSettings.fill Color.lightBlue ]
        [ Canvas.rect
            ( Vector2.getX clone.position, Vector2.getY clone.position )
            width
            height
        ]
