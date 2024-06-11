module Internal exposing (..)

import Iddict exposing (Iddict)
import FastDict as Dict exposing (Dict)
import Set exposing (Set)

{-| Graph's edge.
-}
type alias Edge edge = { content : edge, from : Int, to : Int }

{-| Elm representation of a graph.
-}
type alias Graph vertex edge = Iddict (Vertex vertex edge)

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
empty = Iddict.empty

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

{-| Insert a new edge into the graph - if both ends exist.
-}
insertE : { content : edge, from : Int, to : Int } -> Graph vertex edge -> Graph vertex edge
insertE e g =
    case (Iddict.get e.from g, Iddict.get e.to g) of
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
insertV : vertex -> Graph vertex edge -> (Int, Graph vertex edge)
insertV v g =
    Iddict.insert (emptyV v) g

{-| Remove a vertex from the graph, including all edges that connect the vertex
to its neighbours.
-}
removeV : Int -> Graph vertex edge -> Graph vertex edge
removeV =
    Iddict.remove
