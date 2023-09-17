module Ecs.Entity exposing
    ( Entity
    , add
    , create
    , createWithComponents
    , getByKey
    , remove
    )

import Dict exposing (Dict)
import Ecs.Component as Component exposing (Component)


type alias Entity =
    { id : Int
    , components : Dict Component.Key Component
    , tag : String
    }


create : Int -> String -> Entity
create id tag =
    { id = id, components = Dict.empty, tag = tag }


createWithComponents : Int -> String -> List Component -> Entity
createWithComponents id tag components =
    { id = id
    , components =
        components
            |> List.foldl
                (\component acc ->
                    Dict.insert (Component.toKey component) component acc
                )
                Dict.empty
    , tag = tag
    }


add : Component -> Entity -> Entity
add component entity =
    { entity | components = Dict.insert (Component.toKey component) component entity.components }


remove : Component -> Entity -> Entity
remove component entity =
    { entity | components = Dict.remove (Component.toKey component) entity.components }


getByKey : Component.Key -> Entity -> Maybe Component
getByKey key entity =
    Dict.get key entity.components
