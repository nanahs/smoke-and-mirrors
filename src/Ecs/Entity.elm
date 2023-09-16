module Ecs.Entity exposing
    ( Entity
    , add
    , create
    , getByKey
    , remove
    , update
    )

import Dict exposing (Dict)
import Ecs.Component as Component exposing (Component)


type alias Entity =
    { id : Int
    , components : Dict Component.Key Component
    }


create : Int -> Entity
create id =
    { id = id, components = Dict.empty }


add : Component -> Entity -> Entity
add component entity =
    { entity | components = Dict.insert (Component.toKey component) component entity.components }


remove : Component -> Entity -> Entity
remove component entity =
    { entity | components = Dict.remove (Component.toKey component) entity.components }


update : (Component -> Component) -> Component -> Component
update func component =
    func component


getByKey : Component.Key -> Entity -> Maybe Component
getByKey key entity =
    Dict.get key entity.components
