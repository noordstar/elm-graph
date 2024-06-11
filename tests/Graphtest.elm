module Graphtest exposing (..)

import Test exposing (..)
import Fuzz exposing (Fuzzer)
import Graph exposing (Graph)
import Internal
import Recursion
import Expect

fuzzer : Fuzzer vertex -> Fuzzer edge -> Fuzzer (Graph vertex edge)
fuzzer vf ef =
    Fuzz.andThen (\i -> fuzzerIterate i vf ef) (Fuzz.intRange 0 1024)

fuzzerIterate : Int -> Fuzzer vertex -> Fuzzer edge -> Fuzzer (Graph vertex edge)
fuzzerIterate n vf ef =
    Recursion.runRecursion
        (\i ->
            if i <= 0 then
                Recursion.base (Fuzz.constant Graph.empty)
            else
                Recursion.recurseThen (i - 1)
                    (\graphFuzzer ->
                        graphFuzzer
                            |> Fuzz.andThen (operationFuzzer vf ef)
                            |> Recursion.base
                    )
        )
        n

operationFuzzer : Fuzzer vertex -> Fuzzer edge -> Graph vertex edge -> Fuzzer (Graph vertex edge)
operationFuzzer vf ef g =
    Fuzz.oneOf
        -- Add a new vertex
        [ Fuzz.map
            (\v -> Tuple.second (Graph.insertV v g) )
            vf

        -- Connect two random vertices with an edge
        , case Graph.v g of
            [] ->
                Fuzz.constant g

            _ :: _ ->
                Fuzz.map3
                    (\e f t -> Graph.insertE { content = e, from = f, to = t } g)
                    ef
                    (Fuzz.oneOfValues (Graph.v g))
                    (Fuzz.oneOfValues (Graph.v g))
        
        -- Remove a vertex
        , case Graph.v g of
            [] ->
                Fuzz.constant g
            
            _ :: _ ->
                Fuzz.map (\v -> Graph.removeV v g) (Fuzz.oneOfValues (Graph.v g))
        
        -- Remove an edge
        , case Graph.v g of
            [] ->
                Fuzz.constant g
            
            _ :: _ ->
                Fuzz.map2 (\v1 v2 -> Graph.removeE {from  = v1, to = v2} g)
                    (Fuzz.oneOfValues (Graph.v g))
                    (Fuzz.oneOfValues (Graph.v g))
        ]

suite : Test
suite =
    describe "Graph"
        [ describe "Empty graphs"
            [ test "Empty graph isEmpty"
                (\() ->
                    Graph.empty
                        |> Graph.isEmpty
                        |> Expect.equal True
                )
            , fuzz (fuzzer Fuzz.string Fuzz.string) "Only empty graphs have zero vertices"
                (\g ->
                    Expect.equal
                        (Graph.isEmpty g)
                        (Graph.sizeV g == 0)
                )
            ]
        , describe "Graph sizes"
            [ fuzz (fuzzer Fuzz.string Fuzz.string) "Vertex size matches length of vertex list"
                (\g ->
                    Expect.equal
                        (List.length (Graph.v g))
                        (Graph.sizeV g)
                )
            , fuzz (fuzzer Fuzz.string Fuzz.string) "Edge size matches length of edge list"
                (\g ->
                    Expect.equal
                        (List.length (Graph.e g))
                        (Graph.sizeE g)
                )
            ]
        ]
