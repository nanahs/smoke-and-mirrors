module Input exposing (Input(..), decoder, toString, toVector2)

import Json.Decode as Decode exposing (Decoder)
import Vector2 exposing (Vector2)


type Input
    = Up
    | Down
    | Left
    | Right
    | Shoot



--


toString : Input -> String
toString input =
    case input of
        Up ->
            "Up"

        Down ->
            "Down"

        Left ->
            "Left"

        Right ->
            "Right"

        Shoot ->
            "Shoot"


toVector2 : Input -> Vector2
toVector2 input =
    case input of
        Up ->
            Vector2.create { x = 0, y = 1.0 }

        Down ->
            Vector2.create { x = 0, y = -1 }

        Left ->
            Vector2.create { x = -1, y = 0 }

        Right ->
            Vector2.create { x = 1, y = 0 }

        Shoot ->
            Vector2.zero



-- DECODER


decoder : Decoder Input
decoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "ArrowLeft" ->
                        Decode.succeed Left

                    "ArrowRight" ->
                        Decode.succeed Right

                    "ArrowUp" ->
                        Decode.succeed Up

                    "ArrowDown" ->
                        Decode.succeed Down

                    " " ->
                        Decode.succeed Shoot

                    "a" ->
                        Decode.succeed Left

                    "d" ->
                        Decode.succeed Right

                    "w" ->
                        Decode.succeed Up

                    "s" ->
                        Decode.succeed Down

                    _ ->
                        Decode.fail ""
            )
