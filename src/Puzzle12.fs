namespace AdventOfCode2017

open FSharpx
open System.Collections.Generic

module Puzzle12 =
    module Parsers = 
        open FParsec

        let lineSpaces = many (pchar ' ' <|> tab)
        let pRow = 
            spaces >>. pint32
            .>> spaces .>> pstring "<->"
            .>>. sepBy (spaces >>. pint32) (lineSpaces >>. pchar ',')
        let readPipesLayout input =
            run (many pRow) input
            |> Choice.fromParserResult
    
    type Parent = int
    type Joins = Dictionary<int, Parent>

    let getRootAndOptimizeTrail (joins: Joins) value =
        let rec findRoot leaf optimizeTrail =
            match joins.TryGetValue leaf with
            | false, _ ->
                joins.[leaf] <- leaf
                optimizeTrail leaf
            | true, parent ->
                if parent = leaf
                then optimizeTrail parent
                else 
                    let newOptimizeTrail root = 
                        joins.[leaf] <- root
                        optimizeTrail root
                    findRoot parent newOptimizeTrail
        findRoot value id

    let getRoot (joins: Joins) value =
        let rec findRoot leaf =
            match joins.TryGetValue leaf with
            | false, _ ->
                joins.[leaf] <- leaf
                leaf
            | true, parent ->
                if parent = leaf
                then parent
                else findRoot parent
        findRoot value

    let addJoin (joins: Joins) rootA leafB =
        let rootB = getRootAndOptimizeTrail joins leafB
        if rootA = rootB
        then ()
        else joins.[rootB] <- rootA

    let createGraph input =
        Parsers.readPipesLayout input
        |> Choice.map(fun rows ->
            let joins = Dictionary<int, Parent>()
            rows
            |> List.iter(fun (rowRoot, pipes)->
                let treeRoot = getRootAndOptimizeTrail joins rowRoot
                pipes
                |> List.iter (addJoin joins treeRoot)
            )
            joins)


    let getClusterSize clusterId input=
        createGraph input
        |> Choice.map(fun joins ->
            let clusterIdRoot = getRoot joins clusterId
            joins
            |> Seq.sumBy( fun kvp -> if clusterIdRoot = getRoot joins kvp.Key then 1 else 0))

    let getNumberOfClusters input=
        createGraph input
        |> Choice.map(fun joins ->
            joins
            |> Seq.map( fun kvp -> getRoot joins kvp.Key)
            |> Seq.distinct
            |> Seq.length)

module Puzzle12Tests =
    open Expecto
    open Puzzle12

    let basePuzzle121Test testFunction label clusterId sample expected = testFunction label <| fun _ ->
        getClusterSize clusterId sample
        |> expectSuccess "Failed to read the pipes layout"
        ==? expected
    let basePuzzle122Test testFunction label sample expected = testFunction label <| fun _ ->
        getNumberOfClusters sample
        |> expectSuccess "Failed to read the pipes layout"
        ==? expected
    let puzzleSample = readFile "day12Sample"
    [<Tests>]
    let samples =
        testList
            "Puzzle12 "
            [   testList
                    "Samples 1"
                    [   basePuzzle121Test testCase "Sample 0" 0 puzzleSample 6
                        basePuzzle121Test testCase "Sample 1" 1 puzzleSample 1
                    ]
                basePuzzle122Test testCase "Samples 2/Counting sample" puzzleSample 2
            ]


    let Puzzle12Sample = readFile "day12Input"
    [<Tests>]
    let Puzzle12 =
        testList 
            "Puzzle12 "
            [   basePuzzle121Test testCase "1" 0 Puzzle12Sample 145
                basePuzzle122Test testCase "2" Puzzle12Sample 207 ]