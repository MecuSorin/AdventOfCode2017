namespace AdventOfCode2017

open System
open FSharpx
open System.Collections.Generic
open Hopac

module Puzzle18 =
    type Value =
        | Register of char
        | Number of int64
    type Instruction = 
        | Set of char * Value
        | Add of char * Value
        | Multiply of char * Value
        | Modulo of char * Value
        | Jumps of Value * Value

        | Snd of Value
        | Rcv of char
    
    type Registers = Dictionary<char, int64>
    module Parsers =
        open FParsec

        let pLabel label =  pstring label >>. spaces >>. letter
        let pValue = spaces >>. ((pint64 |>> Number) <|> (letter |>> Register))
        let pSound = pstring "snd" >>.pValue |>> Snd
        let pSet = pLabel "set" .>>. pValue |>> Set  
        let pAdd = pLabel "add" .>>. pValue |>> Add  
        let pMultiply = pLabel "mul" .>>. pValue |>> Multiply  
        let pModulo = pLabel "mod" .>>. pValue |>> Modulo  
        let pRecovers = pLabel "rcv" |>> Rcv  
        let pJumps = pstring "jgz" >>. pValue .>>. pValue |>> Jumps  
        let pInstruction = spaces >>. choice [pSound; pSet; pAdd; pMultiply; pModulo; pRecovers; pJumps]
        let readInstructions input = 
            run (many1 pInstruction) input
            |> Choice.fromParserResult

    module Part1 = 
        type State = 
            {   position: int
                lastSound: int64 }
        let getRecoveredFrequency instructions = 
            let registers = Registers()
            let getRegisterValue r = 
                Map.tryFindd r registers |> Option.defaultValue 0L
            let getValue = function
                | Number n -> n
                | Register r -> getRegisterValue r
            let next state = { state with position = state.position + 1 }
            let rec executeInstructions (state: State) =
                if instructions |> List.length <= state.position || state.position < 0
                then state.lastSound
                else
                    match instructions.[state.position] with
                    | Snd r -> 
                        { position = state.position + 1; lastSound = getValue r }
                    | Set (r, v) -> 
                        registers.[r] <- getValue v
                        next state
                    | Add (r, v) ->
                        registers.[r] <- getRegisterValue r + (getValue v)
                        next state
                    | Multiply (r, v) ->
                        registers.[r] <- getRegisterValue r * getValue v
                        next state
                    | Modulo (r, v) ->
                        registers.[r] <- (getRegisterValue r) % getValue v
                        next state
                    | Rcv r ->
                        if getRegisterValue r <> 0L
                        then { state with position = -1 }
                        else next state
                    | Jumps (r, v) ->
                        if getValue r > 0L
                        then { state with position = state.position + int (getValue v) }
                        else next state
                    |> executeInstructions
            executeInstructions { position = 0; lastSound = 0L}

    module Part2 = 
        open Hopac
        open Hopac.Infixes

        type ValuesPipe = Mailbox<int64>
        type KillPipe = Ch<unit>
        type State = 
            {   position: int
                countSent: int }

        let getEngine 
                    instructions 
                    registers 
                    (clientSendCh: ValuesPipe) 
                    (serverSendCh: ValuesPipe)
                    (clientSendKillCh: KillPipe)
                    (serverSendKillCh: KillPipe)
                     =
            job {
                let result = IVar<int>()
                //#region similar code
                let getRegisterValue r = 
                    Map.tryFindd r registers |> Option.defaultValue 0L
                let getValue = function
                    | Number n -> n
                    | Register r -> getRegisterValue r
                let next state = { state with position = state.position + 1 }
                let stopEngine state =
                    IVar.fill result state.countSent
                    >>= Alt.never   // not needed, is just to show the intent that should not process any more instructions
                let rec executeInstructions (state: State) =
                    job {
                        if instructions |> List.length <= state.position || state.position < 0
                        then 
                            return! stopEngine state
                        else
                            match instructions.[state.position] with
                            | Set (r, v) -> 
                                registers.[r] <- getValue v
                                return! executeInstructions <| next state
                            | Add (r, v) ->
                                registers.[r] <- getRegisterValue r + (getValue v)
                                return! executeInstructions <| next state
                            | Multiply (r, v) ->
                                registers.[r] <- getRegisterValue r * getValue v
                                return! executeInstructions <| next state
                            | Modulo (r, v) ->
                                registers.[r] <- (getRegisterValue r) % getValue v
                                return! executeInstructions <| next state
                                //#endregion
                            | Jumps (r, v) ->
                                let newState =
                                    if getValue r > 0L
                                        then { state with position = state.position + int (getValue v) }
                                        else next state
                                return! executeInstructions newState                                    
                            | Snd r -> 
                                return!
                                    Mailbox.send serverSendCh (getValue r)
                                    >>= fun _ ->
                                            {   position = state.position + 1
                                                countSent = state.countSent + 1 }
                                            |> executeInstructions
                            | Rcv r ->
                                return! 
                                    Alt.choose
                                        [   Mailbox.take clientSendCh
                                                ^=> fun v ->
                                                        registers.[r] <- v
                                                        next state |> executeInstructions 
                                            Ch.take clientSendKillCh
                                                ^=> fun _ -> stopEngine state
                                            Ch.give serverSendKillCh ()
                                                ^=> fun _ -> stopEngine state
                                        ]
                    }
                do! Job.queueIgnore (executeInstructions { countSent = 0; position = 0})
                return result
            }

        let getDuet instructions = 
            let getRegister id = 
                let registers = Registers() 
                registers.['p'] <- id
                registers
            let (mbA, mbB, kA, kB) = Mailbox(), Mailbox(), Ch(), Ch()
            let engine0 = getEngine instructions (getRegister 0L) mbA mbB kA kB
            let engine1 = getEngine instructions (getRegister 1L) mbB mbA kB kA
            engine0 <*> engine1
            >>= fun (ivar0, ivar1) ->
                IVar.read ivar0 <*> IVar.read ivar1

module Puzzle18Tests =
    open Expecto
    open Puzzle18

    let basePuzzle181Test testFunction label sample expected = testFunction label <| fun _ ->
        Parsers.readInstructions sample
        |> expectSuccess "Failed to read the instructions"
        |> Part1.getRecoveredFrequency
        ==? expected
    let basePuzzle182Test testFunction label sample expected = testFunction label <| fun _ ->
        Parsers.readInstructions sample
        |> expectSuccess "Failed to read the instructions"
        |> Part2.getDuet
        |> run
        |> snd
        ==? expected

    let sample = readFile "day18Sample"
    let sample2 = readFile "day18Sample2"
    [<Tests>]
    let samples =
        testList
            "Puzzle18 "
            [   testList
                    "Samples 1"
                    [   basePuzzle181Test testCase "Sample " sample 4L ]
                testList
                    "Samples 2"
                    [   basePuzzle182Test testCase "Sample " sample2 3 ]
            ]


    let Puzzle18Sample = readFile "day18Input"
    [<Tests>]
    let Puzzle18 =
        testList 
            "Puzzle18 "
            [   basePuzzle181Test testCase "1" Puzzle18Sample 9423L
                basePuzzle182Test testCase "2" Puzzle18Sample 7620 ]