module Ecs.World exposing (World, addEntity, getNextEntityId, removeEntity, render, singleton, update)

import AssocSet exposing (Set)
import Canvas
import Dict exposing (Dict)
import Ecs.Component as Component
import Ecs.Entity exposing (Entity)
import Ecs.System as System
import Input exposing (Input)


type alias World =
    { entities : Dict Int Entity
    , nextEntityId : Int
    }


singleton : World
singleton =
    { entities = Dict.empty, nextEntityId = 1 }


addEntity : Entity -> World -> World
addEntity entity world =
    { world
        | entities = Dict.insert entity.id entity world.entities
    }


removeEntity : Entity -> World -> World
removeEntity entity world =
    { world
        | entities = Dict.remove entity.id world.entities
    }


update : Float -> Set Input -> World -> World
update delatTime inputs world =
    { world
        | entities =
            let
                updatedEntities =
                    world.entities
                        |> Dict.filter (\_ entity -> Dict.member Component.velKey entity.components)
                        |> Dict.map (\_ entity -> System.playerVelocity delatTime inputs entity)
                        |> Dict.foldl
                            (\key entity worldEntities ->
                                Dict.update key (\_ -> Just entity) worldEntities
                            )
                            world.entities
            in
            -- TODO SHANAN - I think these needs to apply a system to all entities before moving on to the next sytsem
            Dict.map
                (\_ entity ->
                    entity
                        |> System.playerVelocity delatTime inputs
                        |> System.transform
                )
                world.entities
    }


render : World -> Canvas.Renderable
render world =
    world.entities
        |> Dict.map (\_ -> System.render)
        |> Dict.values
        |> Canvas.group []



-- Get Next Entity Id and increment it


getNextEntityId : World -> ( World, Int )
getNextEntityId world =
    ( { world | nextEntityId = world.nextEntityId + 1 }, world.nextEntityId )
