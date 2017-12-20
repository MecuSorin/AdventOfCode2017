namespace AdventOfCode2017

open System
open FSharpx

module Puzzle14 =
    // quick implementations
    let convertToBinary (intValue: int) = 
        let suffix = Convert.ToString(intValue, 2) |> Seq.toList
        let prefix = List.init (4 - suffix.Length) (fun _ -> '0')
        prefix @ suffix
        |> Seq.toArray

    // not optimal 
    // let getBitCount1 hexaString = 
    //     hexaString
    //     |> Seq.collect (int >> convertToBinary)
    //     |> Seq.sumBy int

    let hammingWeight number =
        let rec shift remaining accResult =
            if remaining = 0
            then accResult
            else shift (remaining >>> 1) (accResult + (remaining &&& 1))
        shift number 0
    
    let getBitCount2 hexaString = 
        hexaString
        |> Seq.sumBy (int >> hammingWeight)

    let getDiskLayout input =
        [0..127]
        |> List.map ((sprintf "%s-%d" input ) >> (Puzzle10.knotHash 256))
        |> Seq.map (fun row ->
            row
            |> Seq.map (fun d -> Convert.ToInt32(string d, 16)))

    let getDiskUsage input =
        getDiskLayout input
        |> Seq.sumBy getBitCount2

    [<AutoOpen>]
    module Clustering = 
        open System.Collections.Generic

        [<Struct>]
        type Position = 
            {   row: int
                col: int }
        type Parent = Position
        type Joins = Dictionary<Position, Parent>

        let pos r c = { row = r; col = c }
        
        let getRootAndTrail (joins: Joins) value =
            let rec findRoot leaf trail =
                match joins.TryGetValue leaf with
                | false, _ ->
                    joins.[leaf] <- leaf
                    leaf :: trail
                | true, parent ->
                    if parent = leaf
                    then parent :: trail
                    else findRoot parent (leaf :: trail)
            findRoot value []

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

        let addJoin (joins: Joins) leafA leafB =
            let rootA = getRoot joins leafA
            let bTrail = getRootAndTrail joins leafB
            if rootA = (bTrail |> List.head)
            then ()
            else 
                bTrail
                |> List.iter (fun pos -> joins.[pos] <- rootA)

    let countTheClusters input = 
        let layout = 
            getDiskLayout input
            |> Seq.toArray
            |> Array.map (Seq.collect convertToBinary >> Seq.toArray)
        let joins = Clustering.Joins()
        let addPosition = getRoot joins >> ignore
        let addLink (v1, p1) (v2, p2)  = 
            if v1 = '1' && v2 = '1'
                then addJoin joins p1 p2
                else ()
        layout
        |> Array.iteri (fun r row ->
            // Add the invidual used squares
            row
            |> Array.iteri (fun c cell ->
                if cell = '1'
                then addPosition (pos r c) |> ignore
                else () )
            // Add joined squares on the row
            // let folder cLeft (items2: char[]) =
            //     addLink (items2.[0], pos r cLeft) (items2.[1], pos r <| cLeft + 1)
            //     cLeft + 1
            // row
            // |> Array.windowed 2
            // |> Array.fold folder 0
            // |> ignore 
            )

        // Add joined squares on the row
        for r = 0 to 127 do
            for c = 0 to 126 do
                addLink (layout.[r].[c], pos r c) (layout.[r].[c + 1], pos r <| c + 1)
        // Add joined squares on the col
        for c = 0 to 127 do
            for r = 0 to 126 do
                addLink (layout.[r].[c], pos r c) (layout.[r + 1].[c], pos (r + 1) c)
        // Here the clustering is done
        joins
        |> Seq.map (fun kvp -> kvp.Key |> getRoot joins)
        |> Seq.distinct
        |> Seq.length

module Puzzle14Tests =
    open Expecto
    open Puzzle14

    let basePuzzle141Test testFunction label sample expected = testFunction label <| fun _ ->
        expected ==? getDiskUsage sample
    let basePuzzle142Test testFunction label sample expected = testFunction label <| fun _ ->
        expected ==? countTheClusters sample

    [<Tests>]
    let samples =
        testList
            "Puzzle14 "
            [   testList
                    "Samples 1"
                    [   basePuzzle141Test testCase "Sample flqrgnkx" "flqrgnkx" 8108 ]
                testList
                    "Samples 2"
                    [   basePuzzle142Test testCase "Sample flqrgnkx" "flqrgnkx" 1242 ]
            ]


    let Puzzle14Sample = "ugkiagan"
    [<Tests>]
    let Puzzle14 =
        testList 
            "Puzzle14 "
            [   basePuzzle141Test testCase "1" Puzzle14Sample 8292
                basePuzzle142Test testCase "2" Puzzle14Sample 1069 ]