module Graph exposing (Graph, Vertex, vLabel, Edge, eLabel, origin, destination)
{-| # Graph

The Graph type helps define relational data.

@docs Graph

A Graph consists of data points (called vertices) that can be connected to each
other.

@docs Vertex, vLabel

Vertices can connect to each other using edges. A connection has a one-way
direction and a label. There can not be more than two edges between two vertices
(one for each direction).

@docs Edge, eLabel, origin, destination

## Create

@docs empty
-}

import Internal
import Set exposing (Set)
import FastDict as Dict

{-| An edge is a relation between two vertices. For example, one person can have
a relationship with another person - and you can use a custom `edge` type to
reveal more about the _type_ of relationship.
-}
type Edge vertex edge =
    Edge { destination : Vertex vertex, edge : edge, origin : Vertex vertex, ptr : Int }

{-| A graph is a large data type containing various data points that are linked
to each other.
-}
type Graph vertex edge =
    Graph (Internal.Graph vertex edge)

{-| A Vertex is a data point within the graph. You can store various bits of
content in it.
-}
type Vertex vertex =
    Vertex { ptr : Int, vertex : vertex }

{-| Get the destination that an edge points to.
-}
destination : Edge vertex edge -> Vertex vertex
destination (Edge e) = e.destination

{-| Finds the label of an edge.
-}
eLabel : Edge vertex edge -> edge
eLabel (Edge e) = e.edge

{-| A new and empty graph containing no vertices.
-}
empty : Graph vertex edge
empty = Graph Internal.empty

{-| Insert a new vertex into the graph.
-}
insertV : vertex -> Graph vertex edge -> (Vertex vertex, Graph vertex edge)
insertV v (Graph g) =
    case Internal.insertV v g of
        ( ptr, newG ) ->
            ( Vertex { ptr = ptr, vertex = v }, Graph newG )

{-| Get the vertex that an edge starts from.
-}
origin : Edge vertex edge -> Vertex vertex
origin (Edge e) = e.origin

{-| Get all the edges that start from a given vertex.
-}
pointsTo : Vertex vertex -> Graph vertex edge -> List (edge, Vertex vertex)
pointsTo (Vertex { ptr }) (Graph g) =
    Internal.getV ptr g
        |> Maybe.map .source
        |> Maybe.withDefault Dict.empty
        |> Dict.toList
        |> List.filterMap
            (\(i, e) ->
                g
                    |> Internal.getV i
                    |> Maybe.map
                        (\vx ->
                            ( e, Vertex { ptr = i, vertex = vx.content })
                        )
            )

{-| Remove a vertex from the graph.
-}
removeV : Vertex vertex -> Graph vertex edge -> Graph vertex edge
removeV (Vertex { ptr }) (Graph g) =
    Graph (Internal.removeV ptr g)

{-| Create a graph with a single vertex in it.
-}
singleton : vertex -> ( Vertex vertex, Graph vertex edge )
singleton v = insertV v empty

{-| Finds the label of a vertex.
-}
vLabel : Vertex vertex -> vertex
vLabel (Vertex v) = v.vertex
