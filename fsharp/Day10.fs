module Day10

open FSharpx
open Xunit
open FsUnit.Xunit

type ParseError =
    | Incomplete of string
    | Corrupted of char
    
type ParseResult = Result<string,ParseError>

let chunksEnds =
    [
        '<','>'
        '{','}'
        '[',']'
        '(',')'
    ] |> Map

let isSecondClosingFirst first second = Map.find first chunksEnds = second

let isOpening c =
    Map.containsKey c chunksEnds 
    
let toClosing c =  Map.find c chunksEnds

let resultFold folder value =
    let resultFolder folder result element =
       Result.bind (fun x -> folder x element) result
    Array.fold (resultFolder folder) (Ok value)

let parseChar alreadyOpened currentChar =
        match (currentChar, alreadyOpened) with
        | c, _ when isOpening c -> Ok (currentChar::alreadyOpened)
        | c, lastOpened::rest when isSecondClosingFirst lastOpened c  -> Ok rest
        | _ -> Corrupted currentChar |> Error
      

let parse line  =
    let seekIncomplete =
           function
           | [] -> line |> Ok
           | notEmpty ->
               List.map toClosing notEmpty
               |> System.String.Concat
               |> Incomplete
               |> Error
    
    line
    |> String.toCharArray
    |> resultFold parseChar []
    |> Result.bind seekIncomplete
    
   
[<Fact>]
let ``empty is a legal navigation line`` () =
    "" |> parse |> should equal (ParseResult.Ok "")
    
[<Fact>]
let ``{ is missing a }`` () =
    "{" |> parse |> should equal (ParseResult.Error <| Incomplete "}" )
    
[<Fact>]
let ``[ is missing a ]`` () =
    "[" |> parse |> should equal (ParseResult.Error <| Incomplete "]" )

[<Fact>]
let ``< is missing a >`` () =
    "<" |> parse |> should equal (ParseResult.Error <| Incomplete ">" )
    
[<Fact>]
let ``( is missing a )`` () =
    "(" |> parse |> should equal (ParseResult.Error <| Incomplete ")" )
    
[<Fact>]
let ``[> is corrupted with > instead of ]`` () =
    "[>" |> parse |> should equal (ParseResult.Error <| Corrupted '>' )
    
[<Fact>]
let ``nesting is ok`` () =
    "{[<()>]}" |> parse |> should equal (ParseResult.Ok <| "{[<()>]}" )
    
[<Fact>]
let ``neighbors are ok`` () =
    "{[]<>()}" |> parse |> should equal (ParseResult.Ok <| "{[]<>()}" )
    
[<Fact>]
let ``corruption along the way`` () =
    "{[]<]()}" |> parse |> should equal (ParseResult.Error <| Corrupted ']' )