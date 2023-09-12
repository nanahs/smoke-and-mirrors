module Constants exposing (gameBounds, gameHeight, gameTransform, gameWidth)

import Canvas.Settings as CanvasSettings
import Canvas.Settings.Advanced as CanvasSettings


gameTransform : CanvasSettings.Setting
gameTransform =
    CanvasSettings.transform [ CanvasSettings.applyMatrix { m11 = 1, m12 = 0, m21 = 0, m22 = -1, dx = 0, dy = 400 } ]


gameHeight : Float
gameHeight =
    400


gameWidth : Float
gameWidth =
    400


gameBounds : Float -> { minX : Float, maxX : Float, minY : Float, maxY : Float }
gameBounds width =
    { minX = 0
    , maxX = gameWidth - width
    , minY = 0
    , maxY = gameHeight / 4
    }
