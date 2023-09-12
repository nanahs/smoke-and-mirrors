module Game exposing (Model, Msg, init, subscriptions, update, view)

import AssocSet as Set exposing (Set)
import BoundingBox exposing (BoundingBox)
import Browser.Events as Events
import Bullet exposing (Bullet)
import Canvas
import Canvas.Settings as CanvasSettings
import Canvas.Settings.Advanced as CanvasSettings
import Color
import Enemy exposing (Enemy)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Input exposing (Input)
import Mirror exposing (Mirror)
import Player exposing (Player)
import Vector2 exposing (Vector2)


type Model
    = Model Internals


type alias Internals =
    { player : Player
    , inputs : Set Input
    , bullets : List Bullet
    , shootTimer : Float
    , cloneCount : Int
    , mirrors : List Mirror
    , hasOverlap : Bool
    , enemies : List Enemy
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        { player = Player.init ( width / 2, height / 10 )
        , inputs = Set.empty
        , bullets = []
        , shootTimer = 0
        , cloneCount = 0
        , mirrors = [ Mirror.init ( 50, 50 ), Mirror.init ( 150, 50 ) ]
        , hasOverlap = False
        , enemies =
            [ Enemy.init ( 25, 350 )
            , Enemy.init ( 75, 375 )
            , Enemy.init ( 125, 325 )
            , Enemy.init ( 300, 350 )
            ]
        }
    , Cmd.none
    )


width : Float
width =
    400


height : Float
height =
    400


shootDelay : Float
shootDelay =
    0.1


canShoot : Internals -> Bool
canShoot model =
    model.shootTimer <= 0


bounds : { minX : Float, maxX : Float, minY : Float, maxY : Float }
bounds =
    { minX = 0
    , maxX = width - Player.width
    , minY = 0
    , maxY = height / 4
    }


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
            in
            ( model.inputs
                |> Set.toList
                |> List.foldl applyMovementInput Vector2.zero
                |> (\velocity ->
                        let
                            newMirrors =
                                List.filter (not << BoundingBox.overlaps (Player.toBounds model.player) << Mirror.toBounds) model.mirrors

                            newEnemies =
                                model.enemies
                                    |> List.filter
                                        (\e ->
                                            not <| List.any (\bullet -> BoundingBox.overlaps (Enemy.toBounds e) (Bullet.toBounds bullet)) model.bullets
                                        )
                        in
                        { model
                            | player =
                                model.player
                                    |> Player.setVelocity velocity
                                    |> Player.updatePosition deltaTime_ bounds
                            , bullets =
                                List.map (Bullet.updatePosition deltaTime_) model.bullets
                            , shootTimer =
                                if not (canShoot model) then
                                    model.shootTimer - deltaTime_

                                else
                                    model.shootTimer
                            , mirrors =
                                newMirrors
                            , hasOverlap =
                                model.mirrors /= newMirrors
                            , cloneCount =
                                model.cloneCount + List.length model.mirrors - List.length newMirrors
                            , enemies =
                                newEnemies
                        }
                   )
                |> Model
            , Cmd.none
            )

        InputDown input ->
            ( Model
                { model
                    | inputs = Set.insert input model.inputs
                    , bullets =
                        if input == Input.Shoot && canShoot model then
                            Bullet.init (Vector2.toTuple (Player.position model.player)) :: model.bullets

                        else
                            model.bullets
                    , shootTimer =
                        if input == Input.Shoot && canShoot model then
                            shootDelay

                        else
                            model.shootTimer
                }
            , Cmd.none
            )

        InputUp input ->
            ( Model { model | inputs = Set.remove input model.inputs }
            , Cmd.none
            )


applyMovementInput : Input -> Vector2 -> Vector2
applyMovementInput input accPos =
    Vector2.add (Input.toVector2 input) accPos


view : Model -> Html Msg
view (Model model) =
    div [ class "flex flex-col items-center" ]
        [ Canvas.toHtml
            ( floor height, floor width )
            [ class "border-2 border-gray-500" ]
            [ clear
            , Canvas.group
                [ CanvasSettings.transform
                    [ CanvasSettings.applyMatrix { m11 = 1, m12 = 0, m21 = 0, m22 = -1, dx = 0, dy = 400 }
                    ]
                ]
                [ Player.render model.player
                , Canvas.group [] (List.map Bullet.render model.bullets)
                , Canvas.group [] (List.map Mirror.render model.mirrors)
                , Canvas.group [] (List.map Enemy.render model.enemies)
                ]
            ]
        , if model.hasOverlap then
            div [] [ text "overlap" ]

          else
            div [] [ text "no" ]
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
    Canvas.shapes [ CanvasSettings.fill Color.white ] [ Canvas.rect ( 0, 0 ) width height ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onAnimationFrameDelta Frame
        , Events.onKeyDown Input.decoder
            |> Sub.map InputDown
        , Events.onKeyUp Input.decoder
            |> Sub.map InputUp
        ]
