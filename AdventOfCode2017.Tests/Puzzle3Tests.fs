namespace AdventOfCode2017

type Direction = 
    | Up
    | Down
    | Left
    | Right

type Position = 
    {   h: int 
        v: int }

module Puzzle3Attempt1 =
    type Position  
        with 
            static member root = { h = 0; v = 0}
            static member stepsToTheRoot position =
                abs(position.h) + abs(position.v)

    type Square = 
        {   width: int
            max: int
            level: int }
        with
            static member Root = 
                {   width = 1
                    max = 1
                    level = 0 }

            member x.NextSquare () =
                let newWidth = x.width + 2
                {   width = newWidth
                    max = x.max + 4 * newWidth  - 4
                    level = x.level + 1 }

    let move steps direction position =
        match direction with
        | Up -> { position with v = position.v + steps}
        | Down -> { position with v = position.v - steps}
        | Left -> { position with h = position.h + steps}
        | Right -> { position with h = position.h - steps}

    let calculateMovement (nr: int) =
        let rec loop square =
            if nr > square.max
            then loop <| square.NextSquare()
            else
                let corner1 = square.max - square.width + 1
                let corner2 = corner1 - square.width + 1
                let corner3 = corner2 - square.width + 1
                let maxPosition = 
                    Position.root
                    |> move square.level Up
                    |> move square.level Left

                if nr > corner1
                then 
                    maxPosition
                    |> move (square.max - nr) Right
                elif nr > corner2
                then
                    maxPosition
                    |> move (square.width - 1) Right
                    |> move (corner1 - nr) Down
                elif nr > corner3
                then
                    maxPosition
                    |> move (square.width - 1) Right
                    |> move (square.width - 1) Down
                    |> move (corner2 - nr) Left
                else
                    maxPosition
                    |> move (square.width - 1) Right
                    |> move (square.width - 1) Down
                    |> move (square.width - 1) Left
                    |> move (corner3 - nr) Up
        loop Square.Root
        |> Position.stepsToTheRoot
module Puzzle3Attempt1Tests =
    open Expecto
    open Puzzle3Attempt1

    let (==?) expected actual = Expect.equal actual expected "Should be equal"

    let basePuzzle31Test testFunction label expected sample = testFunction label <| fun _ ->
        expected ==? calculateMovement sample

    [<Tests>]
    let samples = 
        testList 
            "Puzzle 3 Attempt1"
            [   testList 
                    "Samples 1"
                    [   basePuzzle31Test testCase "Sample 1" 0 1
                        basePuzzle31Test testCase "Sample 12" 3 12
                        basePuzzle31Test testCase "Sample 23" 2 23
                        basePuzzle31Test testCase "Sample 1024" 31 1024 ]
            ]

    [<Tests>]
    let debug = 
        testList 
            "Puzzle 3 Attempt1" 
            [
                testList 
                    "Debug 1/Square lvl 1"
                    [
                        basePuzzle31Test testCase "Debug 9" 2 9
                        basePuzzle31Test testCase "Debug 7" 2 7
                        basePuzzle31Test testCase "Debug 5" 2 5
                        basePuzzle31Test testCase "Debug 3" 2 3
                        basePuzzle31Test testCase "Debug 2" 1 2
                    ]
                testList 
                    "Debug 1/Square lvl 2"
                    [
                        basePuzzle31Test testCase "Debug 24" 3 24
                        basePuzzle31Test testCase "Debug 25" 4 25
                        basePuzzle31Test testCase "Debug 21" 4 21
                        basePuzzle31Test testCase "Debug 17" 4 17
                        basePuzzle31Test testCase "Debug 13" 4 13
                    ]
            ]
    [<Tests>]
    let puzzle3 =
        testList 
            "Puzzle 3 Attempt 1"
            [   basePuzzle31Test testCase "1" 480 347991 ]

module Puzzle3 =
    let makeStep direction position =
        match direction with
        | Up -> { position with v = position.v - 1 }
        | Down -> { position with v = position.v + 1 }
        | Left -> { position with h = position.h - 1 }
        | Right -> { position with h = position.h + 1 }

    let calculateMovement endNo =
        let rec spiral nr pos stepsMade direction squareWidth =
            if nr >= endNo
            then pos
            else
                let goNextCell = makeStep direction
                match direction with
                | Right ->
                    if stepsMade = squareWidth
                    then spiral nr pos 0 Up (squareWidth + 2)
                    else
                        spiral (nr + 1) (goNextCell pos) (stepsMade + 1) direction squareWidth
                | Up ->
                    if stepsMade = squareWidth - 2
                    then spiral nr pos 0 Left squareWidth
                    else
                        spiral (nr + 1) (goNextCell pos) (stepsMade + 1) direction squareWidth
                | Left ->
                    if stepsMade = squareWidth - 1
                    then spiral nr pos 0 Down squareWidth
                    else
                        spiral (nr + 1) (goNextCell pos) (stepsMade + 1) direction squareWidth
                | Down ->
                    if stepsMade = squareWidth - 1
                    then spiral nr pos 0 Right squareWidth
                    else
                        spiral (nr + 1) (goNextCell pos) (stepsMade + 1) direction squareWidth

        spiral 1 { h = 0; v = 0} 0 Right 1
        |> fun p -> abs(p.h) + abs(p.v)

    type State = 
        {   nr: int
            sum: int
            pos: Position
            sumsMap: Map<int, int>
            positionsMap: Map<Position, int> }
        with
            static member Zero = 
                let pos = { h = 0; v = 0}
                {   nr = 1
                    sum = 1
                    pos = pos
                    sumsMap = Map.empty |> Map.add 1 1
                    positionsMap = Map.empty |> Map.add pos 1 }

            member x.nextState direction =
                let newNr = x.nr + 1
                let newPos = makeStep direction x.pos
                let newSum = 
                    [   newPos |> makeStep Up
                        newPos |> makeStep Up |> makeStep Left
                        newPos |> makeStep Up |> makeStep Right
                        newPos |> makeStep Left
                        newPos |> makeStep Right
                        newPos |> makeStep Down
                        newPos |> makeStep Down |> makeStep Left
                        newPos |> makeStep Down |> makeStep Right ]
                    |> List.choose (fun p -> x.positionsMap |> Map.tryFind p)
                    |> List.choose (fun number -> x.sumsMap |> Map.tryFind number)
                    |> List.sum
                {   nr = newNr
                    sum = newSum
                    pos = newPos
                    sumsMap = x.sumsMap |> Map.add newNr newSum 
                    positionsMap = x.positionsMap |> Map.add newPos newNr }


    let findSpiralTreshold endValue = 
        let rec spiral state stepsMade direction squareWidth =
            if state.sum > endValue
            then state.sum
            else
                match direction with
                | Right ->
                    if stepsMade = squareWidth
                    then spiral state 0 Up (squareWidth + 2)
                    else
                        spiral (state.nextState direction) (stepsMade + 1) direction squareWidth
                | Up ->
                    if stepsMade = squareWidth - 2
                    then spiral state 0 Left squareWidth
                    else
                        spiral (state.nextState direction) (stepsMade + 1) direction squareWidth
                | Left ->
                    if stepsMade = squareWidth - 1
                    then spiral state 0 Down squareWidth
                    else
                        spiral (state.nextState direction) (stepsMade + 1) direction squareWidth
                | Down ->
                    if stepsMade = squareWidth - 1
                    then spiral state 0 Right squareWidth
                    else
                        spiral (state.nextState direction) (stepsMade + 1) direction squareWidth

        spiral State.Zero 0 Right 1

module Puzzle3Tests =
    open Expecto
    open Puzzle3

    let (==?) expected actual = Expect.equal actual expected "Should be equal"

    let basePuzzle31Test testFunction label expected sample = testFunction label <| fun _ ->
        expected ==? calculateMovement sample

    let basePuzzle32Test testFunction label endValue expected = testFunction label <| fun _ ->
        expected ==? findSpiralTreshold endValue

    [<Tests>]
    let samples = 
        testList 
            "Puzzle 3"
            [   testList 
                    "Samples 1"
                    [   basePuzzle31Test testCase "Sample 1" 0 1
                        basePuzzle31Test testCase "Sample 12" 3 12
                        basePuzzle31Test testCase "Sample 23" 2 23
                        basePuzzle31Test testCase "Sample 1024" 31 1024 ]
                testList
                    "Samples 2"
                    [   basePuzzle32Test testCase "Sample 1" 1 2
                        basePuzzle32Test testCase "Sample 24" 24 25 ]
            ]

    [<Tests>]
    let debug = 
        testList 
            "Puzzle 3" 
            [
                testList 
                    "Debug 1/Square lvl 1"
                    [
                        basePuzzle31Test testCase "Debug 9" 2 9
                        basePuzzle31Test testCase "Debug 7" 2 7
                        basePuzzle31Test testCase "Debug 5" 2 5
                        basePuzzle31Test testCase "Debug 3" 2 3
                        basePuzzle31Test testCase "Debug 2" 1 2
                    ]
                testList 
                    "Debug 1/Square lvl 2"
                    [
                        basePuzzle31Test testCase "Debug 24" 3 24
                        basePuzzle31Test testCase "Debug 25" 4 25
                        basePuzzle31Test testCase "Debug 21" 4 21
                        basePuzzle31Test testCase "Debug 17" 4 17
                        basePuzzle31Test testCase "Debug 13" 4 13
                    ]
            ]
    [<Tests>]
    let puzzle3 =
        testList 
            "Puzzle 3"
            [   basePuzzle31Test testCase "1" 480 347991
                basePuzzle32Test testCase "2" 347991 349975 ]