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
            -- It is important to apply a whole system one at a time
            world.entities
                |> applySystem [ Component.velKey ] (System.playerVelocity delatTime inputs)
                |> applySystem [ Component.transformKey, Component.velKey ] System.transform
    }


applySystem : List Component.Key -> (Entity -> Entity) -> Dict Int Entity -> Dict Int Entity
applySystem keysRequired system entities =
    entities
        |> Dict.foldl
            (\key entity worldEntities ->
                if List.all (\componentKey -> Dict.member componentKey entity.components) keysRequired then
                    Dict.update key
                        (\_ -> Just (system entity))
                        worldEntities

                else
                    worldEntities
            )
            entities


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
