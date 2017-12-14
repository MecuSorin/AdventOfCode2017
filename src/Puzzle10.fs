namespace AdventOfCode2017

module Puzzle10 =
    type State =
        {   currentPosition: int
            skipSize: int
            numbers: int [] }
        with static member Zero nrs = 
                {   currentPosition = 0
                    skipSize = 0
                    numbers = nrs }
    
    let hash  (initialState: State) (input: int[]) =
        let numbersLength = initialState.numbers.Length
        let safe index = (index + numbersLength) % numbersLength
        let reverseSlice startAt size =
            for offset = 1 to size/2 do
                let a = startAt + offset - 1 |> safe
                let b = startAt + size - offset |> safe
                let c = initialState.numbers.[a]
                initialState.numbers.[a] <- initialState.numbers.[b]
                initialState.numbers.[b] <- c
        
        let folder state length =
            reverseSlice state.currentPosition length
            { state with
                currentPosition = state.currentPosition + length + state.skipSize |> safe 
                skipSize = state.skipSize + 1 }
        input
        |> Array.fold folder initialState

    let extractSign state =
        state.numbers
        |> Array.chunkBySize 16
        |> Array.map ( Array.reduce (^^^) >> (sprintf "%02x"))
        |> String.concat ""
    

module Puzzle10Tests =
    open Expecto
    open Puzzle10

    let basePuzzle101Test testFunction label size (sample: string) expected = testFunction label <| fun _ ->
        sample.Split([|','; ' '; '\t'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> hash (State.Zero [|0 .. size - 1|])
        |> fun s -> s.numbers.[0] * s.numbers.[1]
        ==? expected
    let basePuzzle102Test testFunction label size (sample: string) expected = testFunction label <| fun _ ->
        let lengths =
            sample
            |> System.Text.Encoding.ASCII.GetBytes
            |> Array.map int
            |> (Array.append .|. [|17; 31; 73; 47; 23|])
        [1..64]
        |> List.fold (fun s _ ->
            hash s lengths) (State.Zero [| 0 .. size - 1|])
        |> extractSign
        ==? expected

    [<Tests>]
    let samples =
        testList
            "Puzzle10 "
            [   testList
                    "Samples 1"
                    [   basePuzzle101Test testCase "Sample 3, 4, 1, 5" 5 "3, 4, 1, 5" 12 ]
                testList
                    "Samples 2"
                    [   basePuzzle102Test testCase "Sample empty" 256 "" "a2582a3a0e66e6e86e3812dcb672a272"
                        basePuzzle102Test testCase "Sample AoC 2017" 256 "AoC 2017" "33efeb34ea91902bb2f59c9920caa6cd"
                        basePuzzle102Test testCase "Sample 1,2,3" 256 "1,2,3" "3efbe78a8d82f29979031a4aa0b16a9d" 
                        basePuzzle102Test testCase "Sample 1,2,4" 256 "1,2,4" "63960835bcdc130f0b66d7ff4f6a5a8e" ]
            ]


    let Puzzle10Sample = "189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62"
    [<Tests>]
    let Puzzle10 =
        testList 
            "Puzzle10 "
            [   basePuzzle101Test testCase "1" 256 Puzzle10Sample 38415
                basePuzzle102Test testCase "2" 256 Puzzle10Sample "9de8846431eef262be78f590e39a4848"
            ]

