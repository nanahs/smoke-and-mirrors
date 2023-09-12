module BoundingBox exposing (BoundingBox, contains, fromPointHeightWidth, overlaps)


type alias BoundingBox =
    { minX : Float
    , maxX : Float
    , minY : Float
    , maxY : Float
    }


fromPointHeightWidth : ( Float, Float ) -> Float -> Float -> BoundingBox
fromPointHeightWidth ( x, y ) height width =
    { minX = x
    , maxX = x + width
    , minY = y
    , maxY = y + height
    }


contains : ( Float, Float ) -> BoundingBox -> Bool
contains ( x, y ) box =
    x >= box.minX && x <= box.maxX && y >= box.minY && y <= box.maxY


overlaps : BoundingBox -> BoundingBox -> Bool
overlaps box1 box2 =
    box1.minX <= box2.maxX && box1.maxX >= box2.minX && box1.minY <= box2.maxY && box1.maxY >= box2.minY
