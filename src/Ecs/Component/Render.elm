module Ecs.Component.Render exposing (Render(..))

import Canvas
import Dimensions exposing (Dimensions)
import Vector2 exposing (Vector2)


type Render
    = Render (Vector2 -> Dimensions -> Canvas.Renderable)
