module Smoke exposing (Smoke, generate, generator, init, render)

import Canvas
import Canvas.Settings as CanvasSettings
import Color
import Constants
import Random exposing (Generator)
import Vector2 exposing (Vector2)


type Smoke
    = Smoke Internals


type alias Internals =
    { position : Vector2
    , width : Float
    , height : Float
    }


init : Vector2 -> Float -> Float -> Smoke
init position width height =
    Smoke { position = position, width = width, height = height }


render : Smoke -> Canvas.Renderable
render (Smoke smoke) =
    Canvas.shapes [ CanvasSettings.fill Color.darkGray, CanvasSettings.stroke Color.black ]
        [ Canvas.rect
            ( Vector2.getX smoke.position, Vector2.getY smoke.position )
            smoke.width
            smoke.height
        ]



-- Generators


generate : List Smoke
generate =
    [ ( 10, 20 )
    , ( 25, 25 )
    , ( 60, 20 )
    , ( 80, 20 )
    , ( 100, 20 )
    , ( 120, 40 )
    , ( 140, 40 )
    , ( 160, 40 )
    , ( 180, 40 )
    , ( 200, 40 )
    , ( 220, 60 )
    , ( 240, 60 )
    , ( 260, 60 )
    , ( 280, 60 )
    , ( 300, 60 )
    , ( 320, 80 )
    , ( 340, 80 )
    , ( 360, 80 )
    , ( 380, 80 )
    , ( 400, 80 )
    , ( 420, 100 )
    , ( 440, 100 )
    , ( 460, 100 )
    , ( 480, 100 )
    , ( 500, 100 )
    ]
        |> List.map
            (\( xPos, yPos ) ->
                Smoke { position = Vector2.create { x = xPos, y = Constants.gameHeight - yPos }, width = 30, height = 15 }
            )


generator : Generator (List Smoke)
generator =
    Random.map3
        (\pos width height ->
            Smoke { position = pos, height = height, width = width }
        )
        generatePos
        generateWidth
        generateHeight
        |> Random.list 300


generatePos : Generator Vector2
generatePos =
    Random.pair generateSmokeX generateSmokeY
        |> Random.map
            (\( xPos, yPos ) ->
                Vector2.create { x = xPos, y = yPos }
            )


generateSmokeX : Generator Float
generateSmokeX =
    Random.float 5 (Constants.gameWidth - 45)


generateSmokeY : Generator Float
generateSmokeY =
    Random.float 125 (Constants.gameHeight - 45)


generateWidth : Generator Float
generateWidth =
    Random.float 15 40


generateHeight : Generator Float
generateHeight =
    Random.float 15 40
