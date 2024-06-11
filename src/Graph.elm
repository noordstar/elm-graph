module Graph exposing
    ( Graph
    , Vertex, vLabel
    , Edge, eLabel, origin, destination
    , empty, singleton, insertV, insertE, updateE, removeE, removeV
    , isEmpty, getE, sizeE, sizeV
    , v, e
    , mapV, mapE
    , degree, pointsTo
    )

{-|


# Graph

The Graph type helps define relational data.

@docs Graph

A Graph consists of data points (called vertices) that can be connected to each
other.

@docs Vertex, vLabel

Vertices can connect to each other using edges. A connection has a one-way
direction and a label. There can not be more than two edges between two vertices
(one for each direction).

@docs Edge, eLabel, origin, destination


## Build

@docs empty, singleton, insertV, insertE, updateE, removeE, removeV


## Query

@docs isEmpty, getE, sizeE, sizeV


## Lists

@docs v, e


## Transform

@docs mapV, mapE


## Traverse

@docs degree, pointsTo

-}

import FastDict as Dict
import Internal


{-| An edge is a relation between two vertices. For example, one person can have
a relationship with another person - and you can use a custom `edge` type to
reveal more about the _type_ of relationship.
-}
type Edge vertex edge
    = Edge { destination : Vertex vertex, edge : edge, origin : Vertex vertex }


{-| A graph is a large data type containing various data points that are linked
to each other.
-}
type Graph vertex edge
    = Graph (Internal.Graph vertex edge)


{-| A Vertex is a data point within the graph. You can store various bits of
content in it.
-}
type Vertex vertex
    = Vertex { ptr : Int, vertex : vertex }


{-| Get the destination that an edge points to.
-}
destination : Edge vertex edge -> Vertex vertex
destination (Edge ed) =
    ed.destination


{-| Get the number of outgoing edges for a given vertex.
-}
degree : Vertex vertex -> Graph vertex edge -> Int
degree (Vertex vx) (Graph g) =
    Internal.getV vx.ptr g
        |> Maybe.map (\iv -> Dict.size iv.source)
        |> Maybe.withDefault 0


{-| Get a list of all edges
-}
e : Graph vertex edge -> List (Edge vertex edge)
e (Graph g) =
    Internal.toList g
        |> List.concatMap
            (\( key, vx1 ) ->
                vx1.source
                    |> Dict.toList
                    |> List.filterMap
                        (\( dest, edge ) ->
                            case Internal.getV dest g of
                                Just vx2 ->
                                    Just
                                        (Edge
                                            { destination = Vertex { ptr = dest, vertex = vx2.content }
                                            , edge = edge
                                            , origin = Vertex { ptr = key, vertex = vx1.content }
                                            }
                                        )

                                _ ->
                                    Nothing
                        )
            )


{-| Finds the label of an edge.
-}
eLabel : Edge vertex edge -> edge
eLabel (Edge ed) =
    ed.edge


{-| A new and empty graph containing no vertices.
-}
empty : Graph vertex edge
empty =
    Graph Internal.empty


{-| Get an edge between two vertices, if it exists.
-}
getE : { from : Vertex vertex, to : Vertex vertex } -> Graph vertex edge -> Maybe (Edge vertex edge)
getE data (Graph g) =
    case ( data.from, data.to ) of
        ( Vertex f, Vertex t ) ->
            Internal.getE { from = f.ptr, to = t.ptr } g
                |> Maybe.map
                    (\ed ->
                        Edge
                            { destination = data.to
                            , edge = ed
                            , origin = data.from
                            }
                    )


{-| Insert a new edge into the graph. If the edge already exists, it is
overwritten.
-}
insertE : { content : edge, from : Vertex vertex, to : Vertex vertex } -> Graph vertex edge -> Graph vertex edge
insertE data (Graph g) =
    case ( data.from, data.to ) of
        ( Vertex f, Vertex t ) ->
            Graph
                (Internal.insertE
                    { content = data.content, from = f.ptr, to = t.ptr }
                    g
                )


{-| Checks whether a graph is empty.
-}
isEmpty : Graph vertex edge -> Bool
isEmpty (Graph g) =
    Internal.isEmpty g


{-| Insert a new vertex into the graph.
-}
insertV : vertex -> Graph vertex edge -> ( Vertex vertex, Graph vertex edge )
insertV vx (Graph g) =
    case Internal.insertV vx g of
        ( ptr, newG ) ->
            ( Vertex { ptr = ptr, vertex = vx }, Graph newG )


{-| Map all edge labels to a new value. Note that this function is completely
blind to the location of each edge.
-}
mapE : (edge1 -> edge2) -> Graph vertex edge1 -> Graph vertex edge2
mapE f (Graph g) =
    Graph (Internal.mapE f g)



-- TODO: Give liberty to view environment


{-| Map all vertex labels to a new value. This function cannot change the
structure of the graph, but it can use the structure of the graph to remap the
labels.
-}
mapV : (Vertex vertex1 -> vertex2) -> Graph vertex1 edge -> Graph vertex2 edge
mapV f (Graph g) =
    Graph (Internal.mapV (\i vx -> f (Vertex { ptr = i, vertex = vx })) g)


{-| Get the vertex that an edge starts from.
-}
origin : Edge vertex edge -> Vertex vertex
origin (Edge ed) =
    ed.origin


{-| Get all the edges that start from a given vertex.
-}
pointsTo : Vertex vertex -> Graph vertex edge -> List ( edge, Vertex vertex )
pointsTo (Vertex { ptr }) (Graph g) =
    Internal.getV ptr g
        |> Maybe.map .source
        |> Maybe.withDefault Dict.empty
        |> Dict.toList
        |> List.filterMap
            (\( i, ed ) ->
                g
                    |> Internal.getV i
                    |> Maybe.map
                        (\vx ->
                            ( ed, Vertex { ptr = i, vertex = vx.content } )
                        )
            )


{-| Remove an edge from the graph.
-}
removeE : { from : Vertex vertex, to : Vertex vertex } -> Graph vertex edge -> Graph vertex edge
removeE data (Graph g) =
    case ( data.from, data.to ) of
        ( Vertex f, Vertex t ) ->
            Graph (Internal.removeE { from = f.ptr, to = t.ptr } g)


{-| Remove a vertex from the graph.
-}
removeV : Vertex vertex -> Graph vertex edge -> Graph vertex edge
removeV (Vertex { ptr }) (Graph g) =
    Graph (Internal.removeV ptr g)


{-| Create a graph with a single vertex in it.
-}
singleton : vertex -> ( Vertex vertex, Graph vertex edge )
singleton vx =
    insertV vx empty


{-| Determines the number of edges in a graph.
-}
sizeE : Graph vertex edge -> Int
sizeE (Graph g) =
    Internal.sizeE g


{-| Determines the number of vertices in a graph.
-}
sizeV : Graph vertex edge -> Int
sizeV (Graph g) =
    Internal.sizeV g


{-| Check whether an edge already exists, and update it according to your needs.
-}
updateE : { from : Vertex vertex, to : Vertex vertex } -> (Maybe edge -> Maybe edge) -> Graph vertex edge -> Graph vertex edge
updateE data func (Graph g) =
    case ( data.from, data.to ) of
        ( Vertex f, Vertex t ) ->
            case func (Internal.getE { from = f.ptr, to = t.ptr } g) of
                Just ed ->
                    insertE { content = ed, from = data.from, to = data.to } (Graph g)

                Nothing ->
                    removeE { from = data.from, to = data.to } (Graph g)


{-| Get a list of all vertices.
-}
v : Graph vertex edge -> List (Vertex vertex)
v (Graph g) =
    Internal.toList g
        |> List.map
            (\( key, vx ) ->
                Vertex { vertex = vx.content, ptr = key }
            )


{-| Finds the label of a vertex.
-}
vLabel : Vertex vertex -> vertex
vLabel (Vertex vx) =
    vx.vertex
