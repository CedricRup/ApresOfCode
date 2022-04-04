module Day5

open Xunit
open FsUnit.Xunit


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

let countOverlaps input = 

[<Fact>]
let ``Two vertical ??``() =
    let input =
        [
        ventLine (point (0, 0), point (0, 1))
        ventLine (point (0, 1), point (0, 0))
        ]
    let result = countOverlaps input
    result |> should equal 2

[<Fact>]
let ``Two vertical one point``() =
    let input =
        [
        ventLine (point (0, 0), point (0, 1))
        ventLine (point (0, 1), point (0, 2))
        ]
    let result = countOverlaps input
    result |> should equal 1



//[<Fact>]
let ``Part 1 example``() =
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
    result |> should equal 5