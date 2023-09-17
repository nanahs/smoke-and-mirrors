module Game exposing (Model, Msg, init, subscriptions, update, view)

import AssocSet as Set exposing (Set)
import Browser.Events as Events
import Canvas
import Canvas.Settings as CanvasSettings
import Canvas.Settings.Advanced as CanvasSettings
import Color
import Constants
import Dimensions exposing (Dimensions)
import Ecs.Component as Component
import Ecs.Entity as Entity
import Ecs.World as World exposing (World)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Input exposing (Input)
import Platform.Cmd as Cmd
import Vector2 exposing (Vector2)


type Model
    = Model Internals


type alias Internals =
    { world : World
    , inputs : Set Input
    }


init : ( Model, Cmd Msg )
init =
    let
        ( world, nextEntityId ) =
            World.singleton
                |> World.getNextEntityId

        player =
            Entity.create nextEntityId "Player"
                |> Entity.add (Component.Position Vector2.zero)
                |> Entity.add (Component.Dimensions (Dimensions.init 15 15))
                |> Entity.add (Component.Velocity 30 Vector2.zero)
                |> Entity.add (Component.Render renderPlayer)
                |> Entity.add (Component.Shoot True)
                |> Entity.add Component.Movement
    in
    ( Model
        { world = World.addEntity player world
        , inputs = Set.empty
        }
    , Cmd.none
    )


renderPlayer : Vector2 -> Dimensions -> Canvas.Renderable
renderPlayer position dimensions =
    Canvas.shapes [ CanvasSettings.fill Color.darkBlue ]
        [ Canvas.rect (Vector2.toTuple position) (Dimensions.width dimensions) (Dimensions.height dimensions)
        ]


type Msg
    = Frame Float
    | InputDown Input
    | InputUp Input


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        Frame deltaTime ->
            let
                deltaTime_ =
                    deltaTime * 0.01
            in
            ( Model { model | world = World.update deltaTime_ model.inputs model.world }
            , Cmd.none
            )

        InputDown input ->
            ( Model { model | inputs = Set.insert input model.inputs }
            , Cmd.none
            )

        InputUp input ->
            ( Model { model | inputs = Set.remove input model.inputs }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view (Model model) =
    div [ class "flex flex-col items-center" ]
        [ Canvas.toHtml
            ( floor Constants.gameHeight, floor Constants.gameWidth )
            [ class "border-2 border-gray-500" ]
            [ clear
            , Canvas.group
                [ CanvasSettings.transform
                    [ CanvasSettings.applyMatrix { m11 = 1, m12 = 0, m21 = 0, m22 = -1, dx = 0, dy = 400 } ]
                ]
                [ renderBarrier
                , World.render model.world
                ]
            ]
        , div [] [ text "WASD or Arrow Keys to Move, Space Bar to Shoot" ]
        ]


clear : Canvas.Renderable
clear =
    Canvas.shapes [ CanvasSettings.fill Color.white ]
        [ Canvas.rect ( 0, 0 ) Constants.gameWidth Constants.gameHeight
        ]


renderBarrier : Canvas.Renderable
renderBarrier =
    Canvas.shapes [ CanvasSettings.stroke Color.black ]
        [ Canvas.path ( 0, Constants.gameHeight / 4 + 15 ) [ Canvas.lineTo ( Constants.gameWidth, Constants.gameHeight / 4 + 15 ) ]
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onAnimationFrameDelta Frame
        , Events.onKeyDown Input.decoder
            |> Sub.map InputDown
        , Events.onKeyUp Input.decoder
            |> Sub.map InputUp
        ]
