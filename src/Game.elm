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
import Dict exposing (Dict)
import Enemy exposing (Enemy)
import Game.Action as Action
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Input exposing (Input)
import Mirror exposing (Mirror)
import Platform.Cmd as Cmd
import Player exposing (Player)
import Random
import Smoke exposing (Smoke)
import Vector2


type Model
    = Model Internals


type alias Internals =
    { player : Player
    , inputs : Set Input
    , bullets : Dict String Bullet
    , mirrors : List Mirror
    , enemies : List Enemy
    , clones : List Clone
    , score : Int
    , smokes : List Smoke
    , bulletSpawnId : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        { player = Player.init ( Constants.gameWidth / 2, Constants.gameHeight / 10 )
        , inputs = Set.empty
        , bullets = Dict.empty
        , mirrors = []
        , enemies = Enemy.group
        , clones = []
        , score = 0
        , smokes = []
        , bulletSpawnId = 0
        }
    , Random.generate GotSmoke Smoke.generator
    )


type Msg
    = Frame Float
    | InputDown Input
    | InputUp Input
    | GotSmoke (List Smoke)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        Frame deltaTime ->
            let
                deltaTime_ =
                    deltaTime * 0.001

                addClone =
                    List.length model.mirrors - List.length newMirrors > 0

                ( newPlayer, playerAction ) =
                    Player.update deltaTime_ model.inputs model.player

                ( newClones, cloneActions ) =
                    model.clones
                        |> List.map (Clone.update deltaTime_ model.inputs)
                        |> List.foldl
                            (\( clone, gameAction ) ( clones, actions ) ->
                                ( clone :: clones, gameAction :: actions )
                            )
                            ( [], [] )

                newBullets =
                    (playerAction :: cloneActions)
                        |> List.indexedMap
                            (\index action ->
                                let
                                    nextId =
                                        String.fromInt (model.bulletSpawnId + 1 + index)
                                in
                                case action of
                                    Action.SpawnBullet pos ->
                                        Just (Bullet.init nextId pos)

                                    Action.NoOp ->
                                        Nothing
                            )
                        |> List.filterMap identity

                numClones : Int
                numClones =
                    List.length model.clones

                newMirrors =
                    filterMirrors model.player model.mirrors

                ( newEnemies, bulletIdsToRemove ) =
                    filterEnemiesAndBulletIdsToRemove (Dict.values model.bullets) model.enemies

                newSmoke =
                    filterSmoke (Dict.values model.bullets) model.smokes

                newScore =
                    model.score + List.length model.enemies - List.length newEnemies

                addMirror =
                    modBy 5 newScore == 0
            in
            ( Model
                { model
                    | player = newPlayer
                    , bullets =
                        bulletIdsToRemove
                            |> List.foldl
                                (\bulletId dict ->
                                    Dict.remove bulletId dict
                                )
                                model.bullets
                            |> (\d ->
                                    newBullets
                                        |> List.foldl
                                            (\newBullet dict ->
                                                Dict.insert (Bullet.id newBullet) newBullet dict
                                            )
                                            d
                               )
                            |> Dict.map (\_ -> Bullet.update deltaTime_)
                            |> Dict.filter (\_ -> Bullet.isInBounds)
                    , mirrors =
                        if newScore /= model.score && addMirror then
                            Mirror.init ( 10, 50 ) :: newMirrors

                        else
                            newMirrors
                    , clones =
                        if addClone && List.length model.clones < Constants.maxClones then
                            Clone.init (Vector2.toTuple (Player.position model.player)) (numClones + 1) :: newClones

                        else
                            newClones
                    , enemies = newEnemies
                    , score = newScore
                    , smokes = newSmoke
                    , bulletSpawnId = model.bulletSpawnId + List.length newBullets + 1
                }
            , if List.length model.enemies == 0 then
                Random.generate GotSmoke Smoke.generator

              else
                Cmd.none
            )

        InputDown input ->
            ( Model { model | inputs = Set.insert input model.inputs }
            , Cmd.none
            )

        InputUp input ->
            ( Model { model | inputs = Set.remove input model.inputs }
            , Cmd.none
            )

        GotSmoke smokes ->
            ( Model
                { model
                    | smokes = smokes
                    , enemies = Enemy.group
                }
            , Cmd.none
            )


filterEnemiesAndBulletIdsToRemove : List Bullet -> List Enemy -> ( List Enemy, List String )
filterEnemiesAndBulletIdsToRemove bullets enemies =
    enemies
        |> List.foldl
            (\enemy ( enemyAcc, idsAcc ) ->
                let
                    enemyBulletIdsToRemove =
                        collidedBulletIds enemy bullets
                in
                if List.length enemyBulletIdsToRemove > 0 then
                    ( enemyAcc, List.concat [ enemyBulletIdsToRemove, idsAcc ] )

                else
                    ( enemy :: enemyAcc, idsAcc )
            )
            ( [], [] )


collidedBulletIds : Enemy -> List Bullet -> List String
collidedBulletIds enemy bullets =
    bullets
        |> List.foldl
            (\bullet bulletIdAcc ->
                case collidedBulletId enemy bullet of
                    Just bulletId ->
                        bulletId :: bulletIdAcc

                    Nothing ->
                        bulletIdAcc
            )
            []


collidedBulletId : Enemy -> Bullet -> Maybe String
collidedBulletId e b =
    if BoundingBox.overlaps (Enemy.toBounds e) (Bullet.toBounds b) then
        Just (Bullet.id b)

    else
        Nothing


filterMirrors : Player -> List Mirror -> List Mirror
filterMirrors player mirrors =
    List.filter (not << BoundingBox.overlaps (Player.toBounds player) << Mirror.toBounds) mirrors


filterSmoke : List Bullet -> List Smoke -> List Smoke
filterSmoke bullets smokes =
    smokes
        |> List.filter
            (\smoke ->
                not <|
                    List.any
                        (\bullet ->
                            BoundingBox.overlaps (Smoke.toBounds smoke) (Bullet.toBounds bullet)
                        )
                        bullets
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
                , Player.render model.player
                , Canvas.group [] (List.map Clone.render model.clones)
                , Canvas.group [] (List.map Bullet.render (Dict.values model.bullets))
                , Canvas.group [] (List.map Mirror.render model.mirrors)
                , Canvas.group [] (List.map Enemy.render model.enemies)
                , Canvas.group [] (List.map Smoke.render model.smokes)
                ]
            ]
        , div [ class "flex" ] [ text <| "Score: " ++ String.fromInt (model.score * 10) ]
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
