module Day4

open System
open Xunit
open FsUnit.Xunit


let splitLines (s:string) = s.Split("\n")
let splitBlanks (s:string) = s.Split(" ", StringSplitOptions.RemoveEmptyEntries)



type Square = Number of int | Marked

let squareScore = 
    function
    | Marked -> 0
    | Number i -> i

type Grid = Square[,]

let parseGrid (input : string) =
    input
    |> splitLines 
    |> Array.map (splitBlanks >>  Array.map (int>>Number))
    |> array2D

let calculateGridScore (grid : Grid) =
    grid |> Seq.cast<Square> |> Seq.sumBy squareScore

let markNumber number (grid : Grid) =
    let transform = function
    | Number i when i = number -> Marked
    | a -> a 
    grid |> Array2D.map transform 
    
let gridAsText = @"22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19"

[<Fact>]
let ``Can calculate grid score`` () =
    let grid = parseGrid gridAsText
    let gridScore = calculateGridScore grid
    gridScore |> should equal 300

[<Fact>]
let ``Marking a present number diminish grid score by number`` () =
    parseGrid gridAsText
    |> markNumber 1
    |> calculateGridScore
    |> should equal 299

[<Fact>]
let ``Marking a non present number leaves grid score the same`` () =
    parseGrid gridAsText
    |> markNumber 999
    |> calculateGridScore
    |> should equal 300

