module Day10

open System
open Xunit
open FsUnit.Xunit

let sample = """[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
"""

let areOpposite a b =
        match (a,b) with
        | '<','>' | '{','}' | '[',']' | '(',')' -> true
        | _ -> false

let findFaulty string =
    let folder (_,stack) char =
        match char with
        | '(' | '[' | '{' | '<' -> (None,char::stack)
        | c ->
            match stack with
            | [] -> (Some c,stack)
            | head::tail when areOpposite head c  -> (None,tail)
            | _ -> (Some c,stack)
        
    string
    |> FSharpx.String.toCharArray
    |> Array.scan folder (None,List.empty)
    |> Array.choose fst
    |> Array.tryHead

let syntaxErrorScore = function
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith "Should not happen"

[<Fact>]
let ``Find faulty character`` () =
    "{([(<{}[<>[]}>{[]{[(<()>" |> findFaulty |> should equal (Some '}')
    
let calculateTotalSyntaxErrorScore strings =
    strings
    |> Array.choose (findFaulty >> Option.map syntaxErrorScore) 
    |> Array.sum
    
[<Fact>]
let ``Part 1 sample`` () =
    sample
    |> FSharpx.String.splitString [|"\n"|] StringSplitOptions.RemoveEmptyEntries
    |> calculateTotalSyntaxErrorScore
    |> should equal 26397
    
[<Fact>]
let ``Day 10 part Part 1`` () =
    IO.File.ReadLines "day10Input.txt"
    |> Array.ofSeq
    |> calculateTotalSyntaxErrorScore
    |> should equal 411471

let completeLineScore line =
    let folder stack char =
        match char with
        | '(' | '[' | '{' | '<' -> char::stack
        | c ->
            match stack with
            | head::tail when areOpposite head c  -> tail
            | _ -> failwith "Should not happen"
    let remainder = line |> FSharpx.String.toCharArray |> Array.fold folder List.Empty
    let score = function
        | '(' -> 1L
        | '[' -> 2L
        | '{' -> 3L
        | '<' -> 4L
        | _ -> failwith "Should not happen"
    remainder |> List.fold (fun sum x -> (sum * 5L) + score x) 0L
        
    
[<Fact>]
let ``Complete line score`` () =
    "[({(<(())[]>[[{[]{<()<>>"
    |> completeLineScore
    |> should equal 288957L

let findMiddleScore strings =
    let scores =
        strings
        |> Array.filter (findFaulty >> Option.isNone)
        |> Array.map completeLineScore
        |> Array.sort
    scores[scores.Length / 2]
    

[<Fact>]
let ``Part 2 sample`` () =
    sample
    |> FSharpx.String.splitString [|"\n"|] StringSplitOptions.RemoveEmptyEntries
    |> findMiddleScore
    |> should equal 288957L
    
[<Fact>]
let ``Day 10 part Part 2`` () =
    IO.File.ReadLines "day10Input.txt"
    |> Array.ofSeq
    |> findMiddleScore
    |> should equal 3122628974L

