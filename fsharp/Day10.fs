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

let findFaulty string =
    let areOpposite a b =
        match (a,b) with
        | ('<','>') | ('{','}') | ('[',']') | ('(',')') -> true
        | _ -> false
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

