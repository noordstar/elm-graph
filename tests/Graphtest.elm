module Graphtest exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Graph exposing (Graph)
import Internal
import Recursion
import Test exposing (..)


fuzzer : Fuzzer vertex -> Fuzzer edge -> Fuzzer (Graph vertex edge)
fuzzer vf ef =
    Fuzz.andThen (\i -> fuzzerIterate i vf ef) (Fuzz.intRange 0 1024)


fuzzerIterate : Int -> Fuzzer vertex -> Fuzzer edge -> Fuzzer (Graph vertex edge)
fuzzerIterate n vf ef =
    Recursion.runRecursion
        (\i ->
            if i <= 0 then
                Recursion.base
                    (Fuzz.oneOf
                        [ Fuzz.constant Graph.empty
                        , Fuzz.map (Graph.singleton >> Tuple.second) vf
                        ]
                    )

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
    case Graph.v g of
        [] ->
            Fuzz.map
                (\v -> Tuple.second (Graph.insertV v g))
                vf

        _ :: _ ->
            Fuzz.oneOf
                -- Add a new vertex
                [ Fuzz.map
                    (\v -> Tuple.second (Graph.insertV v g))
                    vf

                -- Connect two random vertices with an edge
                , Fuzz.map3
                    (\e f t -> Graph.insertE { content = e, from = f, to = t } g)
                    ef
                    (Fuzz.oneOfValues (Graph.v g))
                    (Fuzz.oneOfValues (Graph.v g))

                -- Remove a vertex
                , Fuzz.map (\v -> Graph.removeV v g) (Fuzz.oneOfValues (Graph.v g))

                -- Remove an edge
                , Fuzz.map2 (\v1 v2 -> Graph.removeE { from = v1, to = v2 } g)
                    (Fuzz.oneOfValues (Graph.v g))
                    (Fuzz.oneOfValues (Graph.v g))

                -- Update an edge
                , Fuzz.map3
                    (\me v1 v2 ->
                        Graph.updateE
                            { from = v1, to = v2 }
                            (always me)
                            g
                    )
                    (Fuzz.maybe ef)
                    (Fuzz.oneOfValues (Graph.v g))
                    (Fuzz.oneOfValues (Graph.v g))

                -- -- Add a whole bunch of vertices
                -- , Fuzz.listOfLengthBetween 1 8 vf
                --     |> Fuzz.map (List.foldl (\v -> Graph.insertV v >> Tuple.second) g)
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
            , fuzz (fuzzer Fuzz.string Fuzz.string)
                "Only empty graphs have zero vertices"
                (\g ->
                    Expect.equal
                        (Graph.isEmpty g)
                        (Graph.sizeV g == 0)
                )
            ]
        , describe "Graph sizes"
            [ fuzz (fuzzer Fuzz.string Fuzz.string)
                "Vertex size matches length of vertex list"
                (\g ->
                    Expect.equal
                        (List.length (Graph.v g))
                        (Graph.sizeV g)
                )
            , fuzz (fuzzer Fuzz.string Fuzz.string)
                "Edge size matches length of edge list"
                (\g ->
                    Expect.equal
                        (List.length (Graph.e g))
                        (Graph.sizeE g)
                )
            , fuzz (fuzzer Fuzz.string Fuzz.string)
                "Edge size is always at most vertex size squared"
                (\g ->
                    Expect.atMost
                        (Graph.sizeV g ^ 2)
                        (Graph.sizeE g)
                )
            ]
        , describe "Degree"
            [ fuzz (fuzzer Fuzz.string Fuzz.string)
                "Degree matches length of outgoing edges list"
                (\g ->
                    case Graph.v g of
                        [] ->
                            Expect.pass

                        head :: tail ->
                            head
                                :: tail
                                |> List.map
                                    (\v () ->
                                        Expect.equal
                                            (Graph.degree v g)
                                            (List.length <| Graph.pointsTo v g)
                                    )
                                |> Expect.all
                                |> (|>) ()
                )
            , fuzz (fuzzer Fuzz.string Fuzz.string)
                "Degree is always at most graph size"
                (\g ->
                    case Graph.v g of
                        [] ->
                            Expect.pass

                        head :: tail ->
                            head
                                :: tail
                                |> List.map
                                    (\v () ->
                                        Expect.atMost
                                            (Graph.sizeV g)
                                            (Graph.degree v g)
                                    )
                                |> Expect.all
                                |> (|>) ()
                )
            ]
        , describe "Folding"
            [ fuzz (fuzzer Fuzz.int Fuzz.unit)
                "Sum folding is direction-blind"
                (\g ->
                    Expect.equal
                        (Graph.foldl (\v s -> Graph.vLabel v + s) 0 g)
                        (Graph.foldr (\v s -> Graph.vLabel v + s) 0 g)
                )
            , fuzz (fuzzer Fuzz.int Fuzz.unit)
                "Sum increases by vertex size (foldl)"
                (\g ->
                    Expect.equal
                        (Graph.foldl (\v s -> Graph.vLabel v + s) 0 g)
                        (Graph.v g
                            |> List.map Graph.vLabel
                            |> List.sum
                        )
                )
            , fuzz (fuzzer Fuzz.int Fuzz.unit)
                "Sum increases by vertex size (foldr)"
                (\g ->
                    Expect.equal
                        (Graph.foldr (\v s -> Graph.vLabel v + s) 0 g)
                        (Graph.v g
                            |> List.map Graph.vLabel
                            |> List.sum
                        )
                )
            , fuzz (fuzzer Fuzz.int Fuzz.unit)
                "Mapping still has proper folding (foldl)"
                (\g ->
                    Expect.equal
                        (g
                            |> Graph.mapV (\v -> Graph.vLabel v + 1)
                            |> Graph.foldl (\v s -> Graph.vLabel v + s) 0
                        )
                        (g
                            |> Graph.foldl (\v s -> Graph.vLabel v + s) 0
                            |> (+) (Graph.sizeV g)
                        )
                )
            , fuzz (fuzzer Fuzz.int Fuzz.unit)
                "Mapping still has proper folding (foldr)"
                (\g ->
                    Expect.equal
                        (g
                            |> Graph.mapV (\v -> Graph.vLabel v + 1)
                            |> Graph.foldr (\v s -> Graph.vLabel v + s) 0
                        )
                        (g
                            |> Graph.foldr (\v s -> Graph.vLabel v + s) 0
                            |> (+) (Graph.sizeV g)
                        )
                )
            ]
        ]
