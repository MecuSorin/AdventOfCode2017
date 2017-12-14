namespace AdventOfCode2017

open FSharpx
open System.Collections.Generic

module Puzzle =

    type Literal =
        | Register of string
        | Number of int
    and Operation =
        | Noop
        | Increment of Literal * Literal
        | Decrement of Literal * Literal
    and Command =
        | If of Expression * trueOp: Operation * elseOp: Operation
    and Expression =
        | Boolean of Literal * Condition * Literal
    and Condition =
        | GreaterEqual
        | SmallerEqual
        | Greater
        | Smaller
        | Equal
        | NotEqual

    type State = Result<Dictionary<string, int>, string>
    
    module Parsers =
        open FParsec
        let pRegister = spaces >>? many1Chars letter |>> Register
        let pNumber = spaces >>? pint32 |>> Number
        // need to backtrack for every parser in choice
        let pLiteral = pRegister <|> pNumber
        let pIncrement = (pRegister .>> spaces1) .>>? pstring "inc" .>>. pLiteral |>> Increment
        let pDecrement = (pRegister .>> spaces1) .>>? pstring "dec" .>>. pLiteral |>> Decrement
        // need to backtrack for every parser in choice
        let pOperation = pIncrement <|> pDecrement
        let pGreater = pstring ">" >>% Greater
        let pGreaterEqual = pstring ">=" >>% GreaterEqual
        let pSmaller = pstring "<" >>% Smaller
        let pSmallerEqual = pstring "<=" >>% SmallerEqual
        let pEqual = pstring "==" >>% Equal
        let pNotEqual = pstring "!=" >>% NotEqual
        // the order is important!
        let pCondition = spaces >>. choice [pGreaterEqual; pGreater; pSmallerEqual; pSmaller; pEqual; pNotEqual ]
        let pBoolean = tuple3 pLiteral pCondition pLiteral |>> Boolean
        let pIf = 
            pOperation .>>. (spaces1 >>. pstring "if" >>. pBoolean) 
            |>> fun (trueOp, condition) -> If (condition, trueOp, Noop)
        let pCommands = many pIf
        let readGrammar parser input =
            match input |> run parser with
            | Success (command, _, _) -> ChoiceBase.Success command
            | Failure (reason, _, _) -> Fail reason
        let readCommands = readGrammar pCommands
    
    module EvaluateCommands =
        let getLiteralValue (mbState: State) = function
            | Number v -> v
            | Register label ->
                match mbState with
                | Success state ->
                    match state.TryGetValue label with 
                    | true, v -> v
                    | false, _ -> 0
                | Fail reason -> failwithf "The state is invalid %A" reason

        let evaluateExpression (state: State) = function
            | Boolean (aLiteral, condition, bLiteral) ->
                let a = getLiteralValue state aLiteral
                let b = getLiteralValue state bLiteral
                match condition with
                    | GreaterEqual -> a >= b
                    | SmallerEqual -> a <= b
                    | Greater -> a > b
                    | Smaller -> a < b
                    | Equal -> a = b
                    | NotEqual -> a <> b

        let saveState (mbState: State) label value =
            mbState
            |> Choice.map (fun state ->
                state.[label] <- value
                state)

        let doOperation state = function
            | Noop -> state
            | Increment (Register label, literal) -> 
                saveState state label ((getLiteralValue state <| Register label) + (getLiteralValue state literal))
            | Decrement (Register label, literal) -> 
                saveState state label ((getLiteralValue state <| Register label) - (getLiteralValue state literal))
            | _ -> Fail "Invalid syntax, I expected a register on the fist position" 

        let evaluateCommand state = function
            | If(expr, trueOp, elseOp) ->
                if evaluateExpression state expr
                then doOperation state trueOp
                else doOperation state elseOp
        let getMaxRegister (state: State) =
            state
            |> Choice.map (fun s ->
                if s.Count > 0
                then s |> Seq.map (fun kvp -> kvp.Value) |> Seq.max
                else System.Int32.MinValue)
            |> Choice.orDefault System.Int32.MinValue

        let evaluateCommandWithMaxRegister (state, maxRegister: int) ifCommand =
            let newState = evaluateCommand state ifCommand
            let newMax = newState |> getMaxRegister
            newState, max maxRegister newMax

        let evaluateCommands state commands = 
            commands
            |> List.fold evaluateCommandWithMaxRegister (state, System.Int32.MinValue)

    let evaluateRegisters input =
        let initialSetup = Success <| Dictionary<string, int>()
        Parsers.readCommands input
        |> Choice.map (EvaluateCommands.evaluateCommands initialSetup)

    let getHighestRegisterAtTheEnd input = 
        evaluateRegisters input
        |> Choice.map (fst >> EvaluateCommands.getMaxRegister)
    let getHighestRegisterOverall input =
        evaluateRegisters input
        |> Choice.map snd

module PuzzleTests =
    open Expecto
    open Puzzle

    let parseTest testFn label parser sample = testFn label <| fun _ ->
        let actual = Parsers.readGrammar parser sample
        actual
        |> expectSuccess "readed the grammar"
        |> ignore

    let basePuzzle1Test testFunction label sample expected = testFunction label <| fun _ ->
        getHighestRegisterAtTheEnd sample
        |> expectSuccess "readed the grammar and evaluated the commands"
        ==? expected
    let basePuzzle2Test testFunction label sample expected = testFunction label <| fun _ ->
        getHighestRegisterOverall sample
        |> expectSuccess "readed the grammar and evaluated the commands"
        ==? expected

    let sample1 = readFile "day08Sample"

    [<Tests>]
    let samples =
        testList
            "Puzzle "
            [   testList
                    "Samples 1"
                    [   basePuzzle1Test testCase "Sample 1" sample1 1
                        parseTest testCase "Read sample1 grammar" Parsers.pCommands sample1 ]
                testList
                    "Samples 2"
                    [   basePuzzle2Test testCase "Sample 1" sample1 10 ]
            ]

    let puzzleSample = readFile "day08Input"
 
    [<Tests>]
    let puzzle =
        testList 
            "Puzzle "
            [   basePuzzle1Test testCase "1" puzzleSample 7787    // not 933
                basePuzzle2Test testCase "2" puzzleSample 8997  ]