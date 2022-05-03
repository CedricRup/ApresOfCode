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

let parseGrid (input : string) =
    let test =
        input
        |> splitLines
        |> Array.map (toCharArray >> Array.map (string >> int))
    
    test |> array2D

let evaluateRiskLevel grid =
    let xDim = Array2D.length2 grid
    let yDim = Array2D.length1 grid
    let neighbors x y = seq{(x-1,y);(x+1,y);(x,y-1);(x,y+1)}
    let isInGrid (x,y) = x >= 0 && x<xDim && y >= 0 && y<yDim
    let riskLevel x y =
        let level = Array2D.get grid y x
        let isLowPoint = neighbors x y |> Seq.filter isInGrid |> Seq.map (fun (x,y) -> Array2D.get grid y x) |> Seq.forall (fun x -> x > level)
        if isLowPoint then (level+1) else 0
    seq {for x in [0..xDim-1] do
             for y in [0..yDim-1] ->
             riskLevel x y 
        } |> Seq.sum


[<Fact>]
let ``Part 1 sample`` () =
    let grid = parseGrid sample
    grid|> evaluateRiskLevel |> should equal 15
    
[<Fact>]
let ``Day 9 Part 1`` () =
    let text = IO.File.ReadAllText "day9Input.txt"
    let grid = parseGrid text
    grid|> evaluateRiskLevel |> should equal 572