module Day9

open System
open Xunit
open FsUnit.Xunit
open FSharpx.String
let sample = """2199943210
3987894921
9856789892
8767896789
9899965678
"""

let splitLines = splitString  [|"\n"|] StringSplitOptions.RemoveEmptyEntries
let parse (input : string) =
    let test =
        input
        |> splitLines
        |> Array.map (toCharArray >> Array.map (string >> int))
    
    test |> array2D
    
let findLowPoints heightmap =
    let getHeight (x,y)  = Array2D.get heightmap y x
    let xMax = Array2D.length2 heightmap - 1
    let yMax = Array2D.length1 heightmap - 1
    let isLowPoint (x,y) =
        let currentHeight = getHeight (x,y) 
        let isInbounds (x,y) = x >= 0 && x <= xMax && y >= 0 && y <= yMax
        let neighbors = [(x-1,y);(x+1,y);(x,y-1);(x,y+1)] |> List.filter isInbounds
        neighbors |> List.map getHeight |> List.forall (fun height -> height > currentHeight)
    
    [
        for y in 0.. yMax do
        for x in 0.. xMax do
        if isLowPoint(x,y) then yield getHeight (x,y)
    ]
        

[<Fact>]
let ``Can find low points in example`` () =
    sample |> parse |> findLowPoints |> should equal  [1;0;5;5]

let riskLevel =
    List.sumBy (fun x -> x+1) 
    
[<Fact>]
let ``Can calculate total risk level for a heightmap`` () =
    sample |> parse |> findLowPoints |> riskLevel |> should equal  15
    
[<Fact>]
let ``Day 9 Part 1`` () =
    let text = IO.File.ReadAllText "day9Input.txt"
    let grid = parse text
    grid|> findLowPoints |> riskLevel |> should equal 572  