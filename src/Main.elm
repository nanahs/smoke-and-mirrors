module Main exposing (Msg(..), main)

import Browser
import Browser.Events as Events
import Canvas
import Canvas.Settings as CanvasSettings
import Canvas.Settings.Advanced as CanvasSettings
import Color
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events



-- MODEL


type alias Model =
    { count : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { count = 0 }, Cmd.none )


width : Float
width =
    200


height : Float
height =
    200


type Msg
    = Frame Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( { model | count = model.count + 1 }, Cmd.none )



-- VIEWS


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "flex flex-col items-center" ]
        [ Html.h1 [ Attributes.class "text-4xl font-bold text-gray-500" ] [ Html.text "Smoke and Mirrors" ]
        , Canvas.toHtml
            ( 200, 200 )
            [ Attributes.class "border-4 border-black" ]
            [ clear
            , render model.count
            ]
        ]


clear : Canvas.Renderable
clear =
    Canvas.shapes [ CanvasSettings.fill Color.white ] [ Canvas.rect ( 0, 0 ) width height ]


render : number -> Canvas.Renderable
render count =
    let
        size =
            width / 3

        x =
            -(size / 2)

        y =
            -(size / 2)

        rotation =
            degrees (count * 3)

        hue =
            toFloat (count / 4 |> floor |> modBy 100) / 100
    in
    Canvas.shapes
        [ CanvasSettings.transform
            [ CanvasSettings.translate (width / 2) (height / 2)
            , CanvasSettings.rotate rotation
            ]
        , CanvasSettings.fill (Color.hsl hue 0.3 0.7)
        ]
        [ Canvas.rect ( x, y ) size size ]



-- PROGRAM


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = \model -> view model
        , subscriptions = \_ -> Events.onAnimationFrameDelta Frame
        }
