module Dimensions exposing (Dimensions, height, init, width)


type Dimensions
    = Dimensions Internals


type alias Internals =
    { height : Float, width : Float }


init : Float -> Float -> Dimensions
init height_ width_ =
    Dimensions { height = height_, width = width_ }


height : Dimensions -> Float
height (Dimensions dimensions) =
    dimensions.height


width : Dimensions -> Float
width (Dimensions dimensions) =
    dimensions.width
