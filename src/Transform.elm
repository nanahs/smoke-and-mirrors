module Transform exposing (Transform, height, init, position, updatePosition, width)

import Vector2 exposing (Vector2)


type Transform
    = Transform Internals


type alias Internals =
    { height : Float, width : Float, position : Vector2 }


init : Float -> Float -> Vector2 -> Transform
init h w v =
    Transform { height = h, width = w, position = v }


height : Transform -> Float
height (Transform transform) =
    transform.height


width : Transform -> Float
width (Transform transform) =
    transform.width


position : Transform -> Vector2
position (Transform transform) =
    transform.position


updatePosition : (Vector2 -> Vector2) -> Transform -> Transform
updatePosition func (Transform transform) =
    Transform { transform | position = func transform.position }
