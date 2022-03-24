module Day4
open System
open Xunit
open FsUnit.Xunit
open FSharpx.Prelude
open FSharpx.String

type Square = Number of int | Marked

let isMarked = function 
    | Marked -> true 
    | Number _ -> false

let squareScore =
    function
    | Marked -> 0
    | Number i -> i

type Grid = Square[,]

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

let getScoreFromFirstWinningGrid draw (grids : Grid list) =
    let playNumberOnEachGrid (state : int * Grid list) (number:int) =
        let _,grids = state
        (number, grids |> List.map  (markNumber number))

    let distributeDrawAndGrids (lastDrawn,grids) = 
        grids |> List.map (fun g -> (lastDrawn,g)) 

    let toDraw = (-1,grids)

    let isGridBingo = (snd >> ``BINGO?``)

    let calculateBingoScore (lastDrawn,grid) = calculateGridScore grid * lastDrawn 
    
    draw
    |> List.scan playNumberOnEachGrid toDraw
    |> List.collect distributeDrawAndGrids
    |> List.tryFind isGridBingo
    |> Option.map calculateBingoScore


let splitLines = splitString  [|"\r\n"|] StringSplitOptions.None
let splitBlanks = splitString [|" "|] StringSplitOptions.RemoveEmptyEntries
let splitDraw = splitString [|","|]  StringSplitOptions.RemoveEmptyEntries
let splitBlocks = splitString [|"\r\n\r\n"|] StringSplitOptions.RemoveEmptyEntries

let parseGrid (input : string) =
    input
    |> splitLines
    |> Array.map (splitBlanks >>  Array.map (int>>Number))
    |> array2D


let parseInputFile () =
    let theBigString = IO.File.ReadAllText "day4Input.txt"
    let blocks = splitBlocks theBigString
    let draw = blocks[0] |> splitDraw |> Array.toList |> List.map int
    let grids =
        blocks[1..] |> Array.toList |> List.map parseGrid
    (draw,grids)


let gridAsText = @"22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19"

let gridAsText2 = @"3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6"

let gridAsText3 = @"14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"


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
let ``Get score from first winning grid nominal case`` () =
    let grid = parseGrid gridAsText
    getScoreFromFirstWinningGrid [22; 13; 17;  0; 11] [grid] |> should equal (Some (11 * 237))

[<Fact>]
let ``Get score from first winning grid with 0 as last marked number`` () =
    let grid = parseGrid gridAsText
    getScoreFromFirstWinningGrid [22; 13; 17;  11; 0] [grid] |> should equal (Some (0 * 237))


[<Fact>]
let ``Get score from first winning grid with multiple grids`` () =
    let grids =
        [gridAsText; gridAsText2; gridAsText3]
        |> List.map parseGrid
    grids |> getScoreFromFirstWinningGrid [14; 21; 17; 24; 4] |> Option.get |> should equal (4 * 245)


[<Fact>]
let ``Day 4 part 1`` () =
    let (draw,grids)  = parseInputFile()
    grids |> getScoreFromFirstWinningGrid draw |> Option.get |> should equal 55770


let getScoreFromLastWinningGrid draw grids =
    let filterBingoedAndPlayNumberOnEachGrid (state : int * Grid list) (number:int) =
        let _,grids = state
        (number, grids |> List.filter (``BINGO?`` >> not ) |> List.map  (markNumber number) )

    let distributeDrawAndGrids (lastDrawn,grids) = 
        grids |> List.map (fun g -> (lastDrawn,g)) 

    let toDraw = (-1,grids)

    let isGridBingo = (snd >> ``BINGO?``)

    let calculateBingoScore (lastDrawn,grid) = calculateGridScore grid * lastDrawn 
    
    draw
    |> List.scan filterBingoedAndPlayNumberOnEachGrid toDraw
    |> List.collect distributeDrawAndGrids
    |> List.filter isGridBingo
    |> List.tryLast
    |> Option.map calculateBingoScore


[<Fact>]
let ``Get score from last winning grid with multiple grids`` () =
    let grids =
        [gridAsText; gridAsText2; gridAsText3]
        |> List.map parseGrid
    let draw = [7;4;9;5;11;17;23;2;0;14;21;24;10;16;13;6;15;25;12;22;18;20;8;19;3;26;1]
    grids |> getScoreFromLastWinningGrid draw |> Option.get |> should equal 1924

[<Fact>]
let ``Day 4 part 2`` () =
    let (draw,grids)  = parseInputFile()
    grids |> getScoreFromLastWinningGrid draw |> Option.get |> should equal 2980



[<Fact>]
let expampleOfScan () = 
    // Fold =  (Acc -> E -> Acc) ->  Acc -> E list -> Acc
    // Ex: sum = (int -> int -> int) ->  0 -> int list

    //Scan = (Acc -> E -> Acc) ->  Acc -> E list -> Acc list
    // Gives all intermediary values

    let addition = List.fold (+) 0
    let resultat = addition [0;1;2;3]
    resultat |> should equal 6
    List.scan (+) 0 [0;1;2;3] |> should equal  [0;0;1;3;6]

    let tryParse (string:string) =
        match Int32.TryParse(string) with
        | (true,resultat) -> Some resultat
        | (false,_) -> None

    //List.choose 
    let choose chooser list = list |> List.map chooser |> List.filter Option.isSome |> List.map Option.get
    let chooseResult = choose tryParse ["a";"17";"Toto";"Quinze";"53"]
    List.choose tryParse ["a";"17";"Toto";"Quinze";"53"] |> should equal chooseResult
