module Game exposing (Model, Msg, init, subscriptions, update, view)

import AssocSet as Set exposing (Set)
import BoundingBox
import Browser.Events as Events
import Bullet exposing (Bullet)
import Canvas
import Canvas.Settings as CanvasSettings
import Canvas.Settings.Advanced as CanvasSettings
import Clone exposing (Clone)
import Color
import Constants
import Enemy exposing (Enemy)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Input exposing (Input)
import Mirror exposing (Mirror)
import Player exposing (Player)
import Vector2


type Model
    = Model Internals


type alias Internals =
    { player : Player
    , inputs : Set Input
    , bullets : List Bullet
    , mirrors : List Mirror
    , enemies : List Enemy
    , clones : List Clone
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        { player = Player.init ( Constants.gameWidth / 2, Constants.gameHeight / 10 )
        , inputs = Set.empty
        , bullets = []
        , mirrors = [ Mirror.init ( 50, 50 ), Mirror.init ( 100, 100 ) ]
        , enemies =
            [ Enemy.init ( 25, 350 )
            , Enemy.init ( 75, 375 )
            , Enemy.init ( 125, 325 )
            , Enemy.init ( 300, 350 )
            ]
        , clones = []
        }
    , Cmd.none
    )


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
                    deltaTime * 0.001

                newMirrors =
                    filterMirrors model.player model.mirrors

                addClone =
                    List.length model.mirrors - List.length newMirrors > 0

                ( newPlayer, maybePlayerBullet ) =
                    Player.update deltaTime_ model.inputs model.player

                ( newClones, maybeCloneBullets ) =
                    model.clones
                        |> List.map (Clone.update deltaTime_ model.inputs)
                        |> List.foldl
                            (\( clone, bullet ) ( clones, bullets ) ->
                                ( clone :: clones
                                , bullet :: bullets
                                )
                            )
                            ( [], [] )

                newBullets : List Bullet
                newBullets =
                    List.filterMap identity (maybePlayerBullet :: maybeCloneBullets)

                numClones : Int
                numClones =
                    List.length model.clones
            in
            ( Model
                { model
                    | player = newPlayer
                    , bullets =
                        [ newBullets, model.bullets ]
                            |> List.concat
                            |> List.map (Bullet.updatePosition deltaTime_)
                    , mirrors = newMirrors
                    , clones =
                        if addClone && List.length model.clones < Constants.maxClones then
                            Clone.init (Vector2.toTuple (Player.position model.player)) (numClones + 1) :: newClones

                        else
                            newClones
                    , enemies = filterEnemies model.bullets model.enemies
                }
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


filterEnemies : List Bullet -> List Enemy -> List Enemy
filterEnemies bullets enemies =
    enemies
        |> List.filter
            (\enemy ->
                not <|
                    List.any
                        (\bullet ->
                            BoundingBox.overlaps (Enemy.toBounds enemy) (Bullet.toBounds bullet)
                        )
                        bullets
            )


filterMirrors : Player -> List Mirror -> List Mirror
filterMirrors player mirrors =
    List.filter (not << BoundingBox.overlaps (Player.toBounds player) << Mirror.toBounds) mirrors



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
                [ Player.render model.player
                , Canvas.group [] (List.map Clone.render model.clones)
                , Canvas.group [] (List.map Bullet.render model.bullets)
                , Canvas.group [] (List.map Mirror.render model.mirrors)
                , Canvas.group [] (List.map Enemy.render model.enemies)
                ]
            ]
        , viewInputs model.inputs
        ]


viewInputs : Set Input -> Html msg
viewInputs inputs =
    inputs
        |> Set.toList
        |> List.map viewInput
        |> div []


viewInput : Input -> Html msg
viewInput input =
    div [] [ text (Input.toString input) ]


clear : Canvas.Renderable
clear =
    Canvas.shapes [ CanvasSettings.fill Color.white ] [ Canvas.rect ( 0, 0 ) Constants.gameWidth Constants.gameHeight ]



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
