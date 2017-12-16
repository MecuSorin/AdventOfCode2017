namespace AdventOfCode2017

open System.Collections.Generic

module Puzzle7 =
    type Tree = 
        | Leaf of Data
        | Node of Data * Tree []

    and Data =
        {   label:string
            weight: int }
        with
            static member create (l, w) = { label = l; weight = w}

    and ReadRowResult =
        | LooseNode of Data * string list option

    module Parsers =
        open FParsec

        let parseLabel = spaces >>. many1CharsTill letter spaces1
        let parseWeight = between (pstring "(") (pstring ")") pint32 .>> spaces
        let parseArrow = pstring "->" .>> spaces1
        let parseListOfLabels = sepBy (spaces >>. many1Chars letter) (pstring ",") .>> spaces
        let parseArrowAndListOfLabels = parseArrow >>. parseListOfLabels
        let rowParser = 
            parseLabel 
            .>>. parseWeight 
            |>> Data.create
            .>>. opt parseArrowAndListOfLabels 
            |>> LooseNode

        let readRows input = 
            input |> run (many (spaces >>. rowParser)) 
            |> Choice.fromParserResult

    let getLooseRows input =
        Parsers.readRows input
        |> Choice.value

    let findTheRoot (input: string) =
        let rows = getLooseRows input
        let parents =
            rows
            |> List.choose(function
                | LooseNode (d, mbChilds) -> mbChilds |> Option.map (fun _ -> d.label))
        let childs =
            rows
            |> List.choose (function 
                | LooseNode (_, x) -> x)
            |> List.concat
            |> Set.ofList
        parents
        |> List.find (fun x -> not( childs |> Set.contains x))

    let generateTree(input : string) =
        let rows = 
            getLooseRows input
            |> List.map(function
                            | LooseNode(d,  c) -> d.label, (d, c))
            |> Map.ofList

        let mutable root = None
        let partialTrees = Dictionary<string, Tree>()

        let rec getTreeWithRoot currentLabel =
            match partialTrees.TryGetValue currentLabel with
            | true, node -> node
            | false, _ ->
                let (d, mbChilds) = rows.[currentLabel]
                match mbChilds with
                | None ->
                    root <- Some (Leaf d)
                | Some childs ->
                    let treeChilds =
                        childs
                        |> List.toArray
                        |> Array.map getTreeWithRoot
                    root <- Some (Node (d, treeChilds ))
                partialTrees.[currentLabel] <- (Option.get root)
                Option.get root

        rows
        |> Seq.iter (fun kvp -> getTreeWithRoot kvp.Key |> ignore)
        root

    let findUnbalancedNode root =
        let mutable deltaToCorrect = None
        let rec computeNodeWeight node =
            match node with
            | Leaf d -> d, d.weight
            | Node (d, childs) ->
                let childWeights =
                    childs
                    |> Array.map computeNodeWeight
                let partitions =
                    childWeights
                    |> Array.groupBy snd
                    |> Array.map ( fun (_, cList) -> cList |> Array.length, cList |> Array.head)
                if partitions.Length = 1
                then ()
                else
                    let (faulty, rightOnes) =
                        partitions
                        |> Array.partition (fun (itemsInGroup,_) -> 1 = itemsInGroup)
                    let requiredWeight = (rightOnes |> Array.head |> snd |> snd)
                    let (faultyNode, totalFaultyNodeWeight) = faulty |> Array.head |> snd
                    match deltaToCorrect with
                    | None -> deltaToCorrect <- Some (requiredWeight - totalFaultyNodeWeight + faultyNode.weight)
                    | Some _ -> ()
                d, d.weight + ( childWeights |> Array.sumBy snd)
        computeNodeWeight root
        |> ignore
        deltaToCorrect

module Puzzle7Tests =
    open Expecto
    open Puzzle7

    let basePuzzle71Test testFunction label sample expected = testFunction label <| fun _ ->
        expected ==? findTheRoot sample

    let readRowTest testFunction label sample expected = testFunction label <| fun _ ->
        Parsers.readRows sample
        |> expectSuccess "able to read rows"
        |> expectAtLeastOne "a least one thing"
        |> List.head
        ==? expected

    let readRowsTest testFunction label sample expected = testFunction label <| fun _ ->
        let actual =
            Parsers.readRows sample
            |> expectSuccess "able to read rows"
            |> expectAtLeastOne "a least one thing"
        Expect.sequenceEqual actual expected "Failed on reading more rows" 

    let basePuzzle72Test testFunction label sample expected = testFunction label <| fun _ ->
        sample
        |> generateTree
        |> expectSome "I can generate a tree from input"
        |> findUnbalancedNode
        |> expectSome "I can find an imbalanced node"
        ==? expected

    let debugRootForPuzzle72Test testFunction label sample expected = testFunction label <| fun _ ->
        sample
        |> generateTree
        |> expectSome "I can generate a tree from input"
        |> function
            | Leaf d -> d.label
            | Node (d, _) -> d.label
        ==? expected

    let sample1Input = readFile "day07Sample"

    let sample1RowsInput= " ktlj (57)\n fwft (72) -> ktlj, cntj, xhth"
    [<Tests>]
    let samples =
        testList
            "Puzzle7 "
            [   testList
                    "Samples 1"
                    [   basePuzzle71Test testCase "Sample 1" sample1Input "tknk"
                        readRowTest testCase "Sample leaf" " ktlj (57)" (LooseNode <| (Data.create ("ktlj", 57), None))
                        readRowTest testCase "Sample node" " fwft (72) -> ktlj, cntj, xhth" (LooseNode <| (Data.create ("fwft", 72), Some ["ktlj"; "cntj"; "xhth"]))
                        readRowsTest 
                            testCase 
                            "Sample 1 reading 2 rows" 
                            sample1RowsInput
                            [   LooseNode <| (Data.create ("ktlj", 57), None)
                                LooseNode <| (Data.create ("fwft", 72), Some ["ktlj"; "cntj"; "xhth"]) ]
                       ]
                testList
                    "Samples 2"
                    [   basePuzzle72Test testCase "Sample default" sample1Input 60 ]
            ]


    let Puzzle7Sample = readFile "day07Input"
    [<Tests>]
    let Puzzle71 =
        testList 
            "Puzzle7 "
            [   basePuzzle71Test testCase "1" Puzzle7Sample "cyrupz"
                debugRootForPuzzle72Test testCase "Debug root" Puzzle7Sample "cyrupz"
                basePuzzle72Test testCase "2" Puzzle7Sample 193 ]   // not 82978 should be smaller