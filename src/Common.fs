namespace AdventOfCode2017

open FParsec
[<AutoOpen>]
module ChoiceBase =
    type Result<'s,'e> = Choice<'s,'e>
    let inline Success s : Result<_,_> = Choice1Of2 s
    let inline Fail e : Result<_,_> = Choice2Of2 e
    let (|Success|Fail|) = function
        | Choice1Of2 s -> Success s
        | Choice2Of2 e -> Fail e

[<RequireQualifiedAccess>]
module Choice =
    open FSharpx

    let value = function
        | Success s -> s
        | Fail e -> failwithf "Failed to retrieve the value from choice because %A" e
    let concat = function
        | Success s -> s
        | Fail e -> Failure e
    let collect map = Choice.map map >> concat
    let orDefault defaultValue = function
        | Success s -> s
        | Fail _ -> defaultValue
    let fromParserResult (parserResult: ParserResult<'r,'us>) : Choice<'r, string> =
        match parserResult with
        | ParserResult.Success (result, _, _) -> Success result
        | Failure (reason, _, _) -> Fail reason


[<RequireQualifiedAccess>]
module String =
    let split (byChars: #seq<char>) (s: string) =
        s.Split(Seq.toArray byChars, System.StringSplitOptions.RemoveEmptyEntries)


module Map =
    open System.Collections.Generic
    
    let tryFindd key (dictionary: Dictionary<'a, 'b>) =
        match dictionary.TryGetValue key with
        | true, v -> Some v
        | false, _ -> None

[<AutoOpen>]
module Resources =
    open System.IO

    let pathCombine a b = Path.Combine (a, b)
    let findFilePath filePartialPath =
        let generatePath parentFoldersCount =
            [1 .. parentFoldersCount]
            |> List.map (fun _ -> "..")
            |> List.fold pathCombine ""
            |> fun r -> Path.Combine(r, filePartialPath)

        [0 .. 8]
        |> List.map generatePath
        |> List.filter File.Exists
        |> List.tryHead 

    let readFile fileStemName =
        let sourceFile = sprintf "%s.txt" fileStemName
        let source = 
            sourceFile
            |> pathCombine "InputsAndSamples"
            |> pathCombine "src"
            |> pathCombine "."
        match findFilePath source with
        | Some sourcePath -> File.ReadAllText(sourcePath)
        | None -> failwithf "Cannot find the file %s relative to the path %s" source System.Environment.CurrentDirectory

    let (.|.) fn a b = fn b a

[<AutoOpen>]
module ExpectoExtra =
    open Expecto

    let (==?) expected actual = Expect.equal expected actual "Should be equal"
    let expectSome label mbVal =
        Expect.isSome mbVal label
        mbVal.Value 
    let expectSuccess label mbVal =
        Expect.isChoice1Of2 mbVal (sprintf "%s %A" label mbVal)
        mbVal |> Choice.value
    let expectAtLeastOne label collection =
        let result = collection |> Seq.toList
        match result with
        | [] -> 
            Expect.isTrue false label
            []
        | _ ->  result 