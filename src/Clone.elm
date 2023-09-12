module Clone exposing (Clone, init, render, toBounds, update)

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


update : Float -> Set Input -> Clone -> Clone
update delta newInputs (Clone clone) =
    let
        maybevelocity =
            clone.inputs
                |> Queue.head
                |> Maybe.map Input.applyInputs

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



-- VIEW


render : Clone -> Canvas.Renderable
render (Clone clone) =
    Canvas.shapes [ CanvasSettings.fill Color.lightBlue ]
        [ Canvas.rect
            ( Vector2.getX clone.position, Vector2.getY clone.position )
            width
            height
        ]
