module Internal exposing (..)

import FastDict as Dict exposing (Dict)
import Iddict exposing (Iddict)
import Set exposing (Set)


{-| Graph's edge.
-}
type alias Edge edge =
    { content : edge, from : Int, to : Int }


{-| Elm representation of a graph.
-}
type alias Graph vertex edge =
    Iddict (Vertex vertex edge)


{-| Graph's vertex.
-}
type alias Vertex vertex edge =
    { content : vertex
    , sink : Set Int
    , source : Dict Int edge
    }


{-| Add an edge, indicating that the edge ends here.
-}
addEdgePointerToSink : Int -> Vertex vertex edge -> Vertex vertex edge
addEdgePointerToSink key v =
    { v | sink = Set.insert key v.sink }


{-| Add an edge, indicating that the edge starts here.
-}
addEdgePointerToSource : Int -> edge -> Vertex vertex edge -> Vertex vertex edge
addEdgePointerToSource key e v =
    { v | source = Dict.insert key e v.source }


{-| An empty graph containing no vertices or edges.
-}
empty : Graph vertex edge
empty =
    Iddict.empty


{-| An empty vertex containing no metadata other than the provided information.
-}
emptyV : vertex -> Vertex vertex edge
emptyV v =
    { content = v
    , sink = Set.empty
    , source = Dict.empty
    }


{-| Get an edge based on its adjacent vertices.
-}
getE : { from : Int, to : Int } -> Graph vertex edge -> Maybe edge
getE data g =
    g
        |> Iddict.get data.from
        |> Maybe.map .source
        |> Maybe.andThen (Dict.get data.to)


{-| Get a vertex by its id.
-}
getV : Int -> Graph vertex edge -> Maybe (Vertex vertex edge)
getV =
    Iddict.get


isEmpty : Graph vertex edge -> Bool
isEmpty =
    Iddict.isEmpty


{-| Insert a new edge into the graph - if both ends exist.
-}
insertE : Edge edge -> Graph vertex edge -> Graph vertex edge
insertE e g =
    case ( Iddict.get e.from g, Iddict.get e.to g ) of
        ( Just v1, Just v2 ) ->
            g
                |> Iddict.update e.from
                    (always <| Just <| addEdgePointerToSource e.to e.content v1)
                |> Iddict.update e.to
                    (always <| Just <| addEdgePointerToSink e.from v2)

        _ ->
            g


{-| Insert a new vertex into the graph.
-}
insertV : vertex -> Graph vertex edge -> ( Int, Graph vertex edge )
insertV v g =
    Iddict.insert (emptyV v) g


mapE : (edge1 -> edge2) -> Graph vertex edge1 -> Graph vertex edge2
mapE f g =
    Iddict.map
        (\_ v ->
            { content = v.content
            , sink = v.sink
            , source = Dict.map (always f) v.source
            }
        )
        g


mapV : (Int -> vertex1 -> vertex2) -> Graph vertex1 edge -> Graph vertex2 edge
mapV f g =
    Iddict.map (\i v -> { content = f i v.content, sink = v.sink, source = v.source }) g


{-| Remove an edge from the graph.
-}
removeE : { from : Int, to : Int } -> Graph vertex edge -> Graph vertex edge
removeE data g =
    g
        |> Iddict.update data.from
            (Maybe.map (\v -> { v | source = Dict.remove data.to v.source }))
        |> Iddict.update data.to
            (Maybe.map (\v -> { v | sink = Set.remove data.from v.sink }))


{-| Remove a vertex from the graph, including all edges that connect the vertex
to its neighbours.
-}
removeV : Int -> Graph vertex edge -> Graph vertex edge
removeV key g =
    case Iddict.get key g of
        Just vx ->
            g
                |> Set.foldl
                    (\from -> removeE { from = from, to = key })
                |> (|>) vx.sink
                |> Dict.foldl
                    (\to _ -> removeE { from = key, to = to })
                |> (|>) vx.source
                |> Iddict.remove key

        Nothing ->
            g


toList : Graph vertex edge -> List ( Int, Vertex vertex edge )
toList =
    Iddict.toList


sizeE : Graph vertex edge -> Int
sizeE =
    Iddict.foldl (\_ vx s -> s + Dict.size vx.source) 0


sizeV : Graph vertex edge -> Int
sizeV =
    Iddict.size
