namespace AdventOfCode2017

[<RequireQualifiedAccess>]
module Choice =
    open FSharpx

    type Result<'s,'e> = Choice<'s,'e>
    let inline Success s : Result<_,_> = Choice1Of2 s
    let inline Fail e : Result<_,_> = Choice2Of2 e
    let (|Success|Fail|) = function
        | Choice1Of2 s -> Success s
        | Choice2Of2 e -> Fail e
    let value = function
        | Success s -> s
        | Fail e -> failwithf "Failed to retrieve the value from choice because %A" e
    let concat = function
        | Success s -> s
        | Fail e -> Failure e
    let collect map = Choice.map map >> concat

[<AutoOpen>]
module ExpectoExtra =
    open Expecto

    let (==?) expected actual = Expect.equal actual expected "Should be equal"
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