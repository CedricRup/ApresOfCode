module Day4

open System
open Xunit
open FsUnit.Xunit
open FSharpx.Prelude


let splitLines (s:string) = s.Split("\n")
let splitBlanks (s:string) = s.Split(" ", StringSplitOptions.RemoveEmptyEntries)



type Square = Number of int | Marked

let isMarked = function |Marked -> true | Number _ -> false

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
    
let ``BINGO?`` (grid:Grid) =
    
    let hasOneAllMarked linesOrColumns =
        linesOrColumns
        |>List.map (Array.forall isMarked) 
        |> List.exists id
    
    let lines () =
        [0 .. (Array2D.length1 grid - 1)]
        |> List.map (fun l -> grid[l,*])
        |> hasOneAllMarked
        
    
    let columns () =
        [0 .. (Array2D.length2 grid - 1)]
        |> List.map (fun c -> grid[*,c])
        |> hasOneAllMarked
    
    lines () || columns ()
    
let markAllTheseNumbers numbers grid =
    numbers
    |> List.fold (flip markNumber) grid
    
let glop draw (grids : Grid list) =
    let grid = grids.Head
    let folder (state : int * Grid) (number:int) =
        let _,grid = state
        (number, markNumber number grid)
    
    draw
    |> List.scan folder (-1,grid)
    |> List.tryFind (snd >> ``BINGO?``) 
    |> Option.map (fun (lastDrawn,grid) -> calculateGridScore grid * lastDrawn)
    |> Option.get
    
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
    
[<Fact>]
let ``BINGO when first line is all marked`` () =
    parseGrid gridAsText
    |>  markAllTheseNumbers [22;13;17;11;0]
    |> ``BINGO?`` |> should equal true   

    
[<Fact>]
let ``Not BINGO when line partially`` () =
    parseGrid gridAsText
    |>  markAllTheseNumbers [22;13;17;0]
    |> ``BINGO?`` |> should equal false 
 

[<Fact>]  
let ``BINGO when last line is all marked`` () =
    parseGrid gridAsText
    |>  markAllTheseNumbers [1;12;20;15;19]
    |> ``BINGO?`` |> should equal true
    
[<Fact>]  
let ``BINGO when first column is all marked`` () =
    parseGrid gridAsText
    |>  markAllTheseNumbers [22;8;21;6;1]
    |> ``BINGO?`` |> should equal true   
 

[<Fact>]
let ``?`` () =
    let grid = parseGrid gridAsText
    glop [22; 13; 17;  0; 11] [grid] |> should equal (11 * 237)
  
[<Fact>]  
let ``??`` () =
    let grid = parseGrid gridAsText
    glop [22; 13; 17;  11; 0] [grid] |> should equal (0 * 237) 