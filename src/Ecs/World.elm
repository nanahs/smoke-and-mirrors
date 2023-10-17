module Ecs.World exposing
    ( World
    , addEntity
    , addEntityWithComponents
    , getNextEntityId
    , removeEntity
    , singleton
    , update
    )

import AssocSet as Set exposing (Set)
import Dict exposing (Dict)
import Ecs.Component as Component exposing (Component)
import Ecs.System as System exposing (System)
import Input exposing (Input)


type alias Entity =
    Int


type alias World =
    { entities : Set Entity
    , components : Dict Entity (Set Component)
    , nextEntityId : Int
    , systems : List (System Int)
    }


singleton : World
singleton =
    { entities = Set.empty, components = Dict.empty, nextEntityId = 1, systems = [] }


addEntity : Entity -> World -> World
addEntity entity world =
    { world
        | entities = Set.insert entity world.entities
        , components = Dict.insert entity Set.empty world.components
    }


addEntityWithComponents : Entity -> Set Component -> World -> World
addEntityWithComponents entity components world =
    { world
        | entities = Set.insert entity world.entities
        , components = Dict.insert entity components world.components
    }


removeEntity : Entity -> World -> World
removeEntity entity world =
    { world
        | entities = Set.remove entity world.entities
        , components = Dict.remove entity world.components
    }


update : Float -> Set Input -> World -> World
update delatTime inputs world =
    let
        newWorld =
            -- world.entities
            --     |> accrueActions [ Component.shootKey ] (System.shoot inputs)
            --     |> List.foldl
            --         (\action worldAcc ->
            --             case action of
            --                 System.SpawnBullet components ->
            --                     let
            --                         ( nextWorld, nextEntityId ) =
            --                             getNextEntityId worldAcc
            --                         entity =
            --                             Entity.createWithComponents nextEntityId "Bullet" components
            --                     in
            --                     nextWorld
            --                         |> addEntity entity
            --                 System.NoOp ->
            --                     worldAcc
            --         )
            world
    in
    { newWorld
        | entities =
            -- It is important to apply a whole system one at a time
            newWorld.entities
                |> applySystem [ Component.velKey ] (System.movement inputs)

        -- -- |> applySystem [ Component.velKey ] (System.velocity delatTime)
        -- |> applySystem [ Component.positionKey, Component.velKey ] (System.stepPosition delatTime)
    }


applySystem : List Component.Key -> (Entity -> Entity) -> Dict Int Entity -> Dict Int Entity
applySystem keysRequired system entities =
    entities
        |> Dict.foldl
            (\key entity worldEntities ->
                if List.all (\componentKey -> Dict.member componentKey entity.components) keysRequired then
                    let
                        _ =
                            Debug.log "entity tag: " entity.tag
                    in
                    Dict.update key
                        (\_ -> Just (system entity))
                        worldEntities

                else
                    worldEntities
            )
            entities



-- accrueActions : List Component.Key -> (Entity -> System.Action) -> Dict Int Entity -> List System.Action
-- accrueActions keysRequired system entities =
--     entities
--         |> Dict.foldl
--             (\_ entity actions ->
--                 if List.all (\componentKey -> Dict.member componentKey entity.components) keysRequired then
--                     system entity :: actions
--                 else
--                     actions
--             )
--             []
-- render : World -> Canvas.Renderable
-- render world =
--     world.entities
--         |> Dict.map (\_ -> System.render)
--         |> Dict.values
--         |> Canvas.group []
-- Get Next Entity Id and increment it


getNextEntityId : World -> ( World, Int )
getNextEntityId world =
    ( { world | nextEntityId = world.nextEntityId + 1 }, world.nextEntityId )
