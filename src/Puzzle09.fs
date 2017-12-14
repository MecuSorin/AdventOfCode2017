namespace AdventOfCode2017

module Puzzle9 =
    type River =
        | Group of River List
        | Garbage of GarbageKind list
    and GarbageKind =
        | Char of char
        | Ignored of char

    module Parsers =
        open FParsec

        module GarbageImpl =
            let pIgnored = pchar '!' >>. anyChar |>> Ignored
            let pAny = anyChar |>> Char
            let pGarbageToken = pIgnored <|> pAny
            // need to change the state so I will not generate Garbage inside Garbage
            let pGarbage = pchar '<' >>. manyTill pGarbageToken (pchar '>')
        
        let pGarbage = GarbageImpl.pGarbage |>> Garbage
        
        
        let pGroupPlaceHolder, pGroupMutable = createParserForwardedToRef()
        let pGroupContent = sepBy  (pGarbage <|> pGroupPlaceHolder) (pchar ',') |>> Group
        let pGroup = pchar '{' >>. pGroupContent .>> pchar '}'
        do pGroupMutable := pGroup

        let readUsing parser sample =
            match sample |> run parser with
            | Success (river, _, _) -> ChoiceBase.Success river
            | Failure (reason, _, _) -> Fail reason
        

    let countGroups rootGroup =
        let rec loop level = function
            | Garbage _ -> 0
            | Group subGroups ->
                subGroups
                |> List.sumBy (loop (level + 1))
                |> fun v -> v + level
        loop 1 rootGroup

    let countGarbage rootGroup =
        let rec loop = function
            | Garbage listOfGarbage -> 
                listOfGarbage
                |> List.sumBy (function 
                    | Char _ -> 1
                    | _ -> 0)
            | Group subGroups ->
                subGroups
                |> List.sumBy loop
        loop rootGroup

module Puzzle9Tests =
    open Expecto
    open Puzzle9

    let parseTest parser label testFunction sample = testFunction (sprintf "%s %s" label sample) <| fun _ ->
        Parsers.readUsing parser sample
        |> expectSuccess "failed to parse the sample"
        |> ignore

    let parseGroupTest = parseTest Parsers.pGroup "Sample group"
    let parseGarbageTest = parseTest Parsers.pGarbage "Sample garbage"
    
    let basePuzzle91PuzzleTest testFunction label sample expected = testFunction label <| fun _ ->
        Parsers.readUsing Parsers.pGroup sample
        |> expectSuccess "failed to parse the sample"
        |> countGroups
        ==? expected

    let basePuzzle91Test testFunction sample expected = testFunction (sprintf "Sample group counting %s" sample) <| fun _ ->
        Parsers.readUsing Parsers.pGroup sample
        |> expectSuccess "failed to parse the sample"
        |> countGroups
        ==? expected

    let basePuzzle92Test testFunction label sample expected = testFunction label <| fun _ ->
        Parsers.readUsing Parsers.pGroup sample
        |> expectSuccess "failed to parse the sample"
        |> countGarbage
        ==? expected

    [<Tests>]
    let samples =
        testList
            "Puzzle9 "
            [   testList
                    "Samples 1"
                    [   
                        basePuzzle91Test testCase "{}" 1
                        basePuzzle91Test testCase "{{{}}}" 6
                        basePuzzle91Test testCase "{{},{}}" 5
                        basePuzzle91Test testCase "{{{},{},{{}}}}" 16
                        basePuzzle91Test testCase "{<a>,<a>,<a>,<a>}" 1
                        basePuzzle91Test testCase "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9
                        basePuzzle91Test testCase "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9
                        basePuzzle91Test testCase "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3
                        parseGarbageTest testCase "<>"
                        parseGarbageTest testCase "< asfasdlfjasdf ksf sdkf>"
                        parseGarbageTest testCase "<<<<>"
                        parseGarbageTest testCase "<{!>}>"
                        parseGarbageTest testCase "<!!>"
                        parseGarbageTest testCase "<!!!>>"
                        parseGarbageTest testCase """<{o"i!a,<{i<a>"""
                        parseGroupTest testCase """{}"""
                        parseGroupTest testCase """{{{}}}"""
                        parseGroupTest testCase """{{},{}}"""
                        parseGroupTest testCase """{{{},{},{{}}}}"""
                        parseGroupTest testCase """{<{},{},{{}}>}"""
                        parseGroupTest testCase """{<a>,<a>,<a>,<a>}"""
                        parseGroupTest testCase """{{<a>},{<a>},{<a>},{<a>}}"""
                        parseGroupTest testCase """{{<!>},{<!>},{<!>},{<a>}}"""
                        parseGroupTest testCase """{{<!!>},{<!!>},{<!!>},{<!!>}}"""
                        parseGroupTest testCase """{{<a!>},{<a!>},{<a!>},{<ab>}}"""

                    ]
                testList
                    "Samples 2"
                    [   basePuzzle92Test testCase "Sample <>" "{<>}" 0
                        basePuzzle92Test testCase "Sample <random characters>" "{<random characters>}" 17
                        basePuzzle92Test testCase "Sample <<<<>" "{<<<<>}" 3
                        basePuzzle92Test testCase "Sample <{!>}>" "{<{!>}>}" 2
                        basePuzzle92Test testCase "Sample <!!>" "{<!!>}" 0
                        basePuzzle92Test testCase "Sample <!!!>>" "{<!!!>>}" 0
                        basePuzzle92Test testCase """Sample <{o"i!a,<{i<a>""" """{<{o"i!a,<{i<a>}""" 10
                    ]
            ]


    let Puzzle9Sample = readFile "day09Input"
    [<Tests>]
    let Puzzle9 =
        testList 
            "Puzzle9 "
            [   basePuzzle91PuzzleTest testCase "1" Puzzle9Sample 16827
                basePuzzle92Test testCase "2" Puzzle9Sample 7298 ]