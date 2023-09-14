module Enemy exposing (Enemy, group, init, render, toBounds)

import BoundingBox exposing (BoundingBox)
import Canvas
import Canvas.Settings as CanvasSettings
import Color
import Constants
import Vector2 exposing (Vector2)



-- Constants


height : Float
height =
    12


width : Float
width =
    18


type Enemy
    = Enemy Internals


type alias Internals =
    { position : Vector2
    }


init : ( Float, Float ) -> Enemy
init ( x, y ) =
    Enemy { position = Vector2.create { x = x, y = y } }


toBounds : Enemy -> BoundingBox
toBounds (Enemy enemey) =
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


render : Enemy -> Canvas.Renderable
render (Enemy enemy) =
    Canvas.shapes [ CanvasSettings.fill Color.red ]
        [ Canvas.rect
            ( Vector2.getX enemy.position, Vector2.getY enemy.position )
            width
            height
        ]



-- Enemy Groups


group : List Enemy
group =
    [ init ( 36, Constants.gameHeight - 25 )
    , init ( 72, Constants.gameHeight - 25 )
    , init ( 108, Constants.gameHeight - 25 )
    , init ( 144, Constants.gameHeight - 25 )
    , init ( 180, Constants.gameHeight - 25 )
    , init ( 216, Constants.gameHeight - 25 )
    , init ( 252, Constants.gameHeight - 25 )
    , init ( 288, Constants.gameHeight - 25 )
    , init ( 324, Constants.gameHeight - 25 )

    --
    , init ( 18, Constants.gameHeight - 75 )
    , init ( 54, Constants.gameHeight - 75 )
    , init ( 90, Constants.gameHeight - 75 )
    , init ( 126, Constants.gameHeight - 75 )
    , init ( 162, Constants.gameHeight - 75 )
    , init ( 198, Constants.gameHeight - 75 )
    , init ( 234, Constants.gameHeight - 75 )
    , init ( 270, Constants.gameHeight - 75 )
    , init ( 306, Constants.gameHeight - 75 )
    , init ( 342, Constants.gameHeight - 75 )

    --
    , init ( 36, Constants.gameHeight - 125 )
    , init ( 72, Constants.gameHeight - 125 )
    , init ( 108, Constants.gameHeight - 125 )
    , init ( 144, Constants.gameHeight - 125 )
    , init ( 180, Constants.gameHeight - 125 )
    , init ( 216, Constants.gameHeight - 125 )
    , init ( 252, Constants.gameHeight - 125 )
    , init ( 288, Constants.gameHeight - 125 )
    , init ( 324, Constants.gameHeight - 125 )
    ]
