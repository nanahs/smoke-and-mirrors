module Pool exposing (Pool, add, init, update)


type Pool a
    = Pool (Internals a)


type Status
    = On
    | Off


type alias Internals a =
    { items : List ( a, Status )
    , limit : Int
    }


init : a -> Int -> Pool a
init item limit =
    Pool { items = List.repeat limit ( item, Off ), limit = limit }


update : Float -> (Float -> a -> a) -> Pool a -> Pool a
update delta updateFunc (Pool pool) =
    Pool
        { pool
            | items =
                List.map
                    (\item ->
                        if Tuple.second item == On then
                            Tuple.mapFirst (updateFunc delta) item

                        else
                            item
                    )
                    pool.items
        }


add : a -> Pool a -> Pool a
add item (Pool pool) =
    Pool pool
