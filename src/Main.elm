module Main exposing (Msg(..), main)

import Browser
import Game
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MODEL


type State
    = Screen
    | Game Game.Model


init : ( State, Cmd Msg )
init =
    ( Screen
    , Cmd.none
    )


type Msg
    = GameMsg Game.Msg
    | ClickedStart
    | NoOp


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case ( msg, state ) of
        ( GameMsg gameMsg, Game game ) ->
            Game.update gameMsg game
                |> Tuple.mapBoth Game (Cmd.map GameMsg)

        ( ClickedStart, Screen ) ->
            Game.init
                |> Tuple.mapBoth Game (Cmd.map GameMsg)

        ( NoOp, _ ) ->
            ( state, Cmd.none )

        _ ->
            ( state, Cmd.none )



-- VIEWS


view : State -> Html Msg
view state =
    case state of
        Screen ->
            Html.div [ class "flex flex-col items-center" ]
                [ Html.h1 [ class "text-4xl font-bold text-gray-500" ] [ Html.text "Smoke and Mirrors" ]
                , Html.p [] [ text "Made for elm-game-jam-6" ]
                , button
                    [ onClick ClickedStart
                    , class "border px-2 rounded-md"
                    ]
                    [ text "Start" ]
                ]

        Game game ->
            Game.view game
                |> Html.map GameMsg



-- PROGRAM


main : Program () State Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = \model -> view model
        , subscriptions =
            \state ->
                case state of
                    Screen ->
                        Sub.none

                    Game game ->
                        Game.subscriptions game
                            |> Sub.map GameMsg
        }
