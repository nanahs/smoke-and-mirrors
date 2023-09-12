module Constants exposing (gameTransform)

import Canvas.Settings as CanvasSettings
import Canvas.Settings.Advanced as CanvasSettings


gameTransform : CanvasSettings.Setting
gameTransform =
    CanvasSettings.transform [ CanvasSettings.applyMatrix { m11 = 1, m12 = 0, m21 = 0, m22 = -1, dx = 0, dy = 400 } ]
