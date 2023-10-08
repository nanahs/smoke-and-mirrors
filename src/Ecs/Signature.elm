module Ecs.Signature exposing (Signature)

import AssocSet exposing (Set)
import Ecs.Component exposing (Component)


type alias Signature =
    { components : Set Component }
