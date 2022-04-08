module Day5

open Xunit
open FsUnit.Xunit
open FParsec.CharParsers
open FParsec.Primitives


type Point = {
    x:int
    y:int
}

type VentLine = {
    start : Point
    finish: Point
}

let point (x,y) = {x=x;y=y}
let ventLine (start,finish) = {start=start;finish=finish} 


let (|Vertical|_|) ventLine =
    let {start={x=xs;y=ys};finish={x=xf;y=yf}} = ventLine
    if xs = xf then Some(xs,[min ys yf .. max ys yf])  else None

let (|Horizontal|_|) ventLine =
    let {start={x=xs;y=ys};finish={x=xf;y=yf}} = ventLine
    if ys = yf then Some([min xs xf .. max xs xf],ys)  else None

let (|Diagonal|) ventLine =
    let {start={x=xs;y=ys} as start;finish={x=xf;y=yf} as finish} = ventLine
    let horizontalDirection = if (xs < xf) then 1 else -1    
    let verticalDirection = if (ys < yf) then 1 else -1
    (start,finish,horizontalDirection,verticalDirection)
    
let toPoints =
    function
    | Vertical (x,ys) -> ys |> List.map (fun y -> point(x,y))
    | Horizontal (xs,y) -> xs |> List.map (fun x -> point(x,y))
    | Diagonal(start, finish,horizontalDirection,verticalDirection) ->
        let horizontals = [start.x .. horizontalDirection ..finish.x]
        let verticals = [start.y .. verticalDirection ..finish.y]
        List.zip horizontals verticals |> List.map point 
    
let countOverlaps input =
    input
    |> List.collect toPoints
    |> List.countBy id
    |> List.filter (fun (_,count) -> count >= 2)
    |> List.length

[<Fact>]
let ``Two vertical fully overlaping, with start and finish switched``() =
    let input =
        [
        ventLine (point (0, 0), point (0, 1))
        ventLine (point (0, 1), point (0, 0))
        ]
    let result = countOverlaps input
    result |> should equal 2


[<Fact>]
let ``Two vertical fully overlaping, with start and finish switched not on x=0``() =
    let input =
        [
        ventLine (point (1, 0), point (1, 1))
        ventLine (point (1, 1), point (1, 0))
        ]
    let result = countOverlaps input
    result |> should equal 2
    
[<Fact>]
let ``Two vertical not overlaping``() =
    let input =
        [
        ventLine (point (1, 0), point (1, 1))
        ventLine (point (2, 1), point (2, 0))
        ]
    let result = countOverlaps input
    result |> should equal 0


[<Fact>]
let ``Two vertical one point``() =
    let input =
        [
        ventLine (point (0, 0), point (0, 1))
        ventLine (point (0, 1), point (0, 2))
        ]
    let result = countOverlaps input
    result |> should equal 1

[<Fact>]
let ``Two horizontal with overlapping one point``() =
    let input =
        [
        ventLine (point (0, 0), point (1, 0))
        ventLine (point (1, 0), point (2, 0))
        ]
    let result = countOverlaps input
    result |> should equal 1


[<Fact>]
let ``Part 2 example``() =
    let input =
        [
        ventLine (point (0, 9), point (5, 9))
        ventLine (point (8, 0), point (0, 8))
        ventLine (point (9, 4), point (3, 4))
        ventLine (point (2, 2), point (2, 1))
        ventLine (point (7, 0), point (7, 4))
        ventLine (point (6, 4), point (2, 0))
        ventLine (point (0, 9), point (2, 9))
        ventLine (point (3, 4), point (1, 4))
        ventLine (point (0, 0), point (8, 8))
        ventLine (point (5, 5), point (8, 2))
        ]
    let result = countOverlaps input
    result |> should equal  12

let pointParser = pipe3 pint32 (pstring ",") (pint32 .>>spaces) (fun x _ y -> point (x,y))
let ventParser = pipe3 pointParser (pstring "->" .>> spaces) pointParser (fun start _ finish -> ventLine (start,finish))

let inputParser = many ventParser

[<Fact>]
let ``Day 5 part 2``() =
    let theBigString = System.IO.File.ReadAllText "day5Input.txt"
    let input = run inputParser theBigString
    let parseResult =
            match input with
            | Success(ventLines, _, _) -> ventLines
            | Failure(s, _, _) -> failwith s
    parseResult |> countOverlaps |> should equal 17717
    
