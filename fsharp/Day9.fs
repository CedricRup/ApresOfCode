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
    input
    |> splitLines
    |> Array.map (toCharArray >> Array.map (string >> int))
    |> array2D

let sampleGrid = parseGrid sample

let neighbors grid x y =
    let xDim = Array2D.length2 grid
    let yDim = Array2D.length1 grid
    let neighbors = seq{(x-1,y);(x+1,y);(x,y-1);(x,y+1)}
    let isInGrid (x,y) = x >= 0 && x<xDim && y >= 0 && y<yDim
    neighbors |> Seq.filter isInGrid

let getLowPoints grid =
    let xDim = Array2D.length2 grid
    let yDim = Array2D.length1 grid
    let neighbors = neighbors grid
    seq {for x in [0..xDim-1] do
             for y in [0..yDim-1] do
                 let level = Array2D.get grid y x
                 let isLowPoint = neighbors x y |> Seq.map (fun (x,y) -> Array2D.get grid y x) |> Seq.forall (fun x -> x > level)
                 if isLowPoint then yield x,y }
   


let evaluateRiskLevel grid =
      let riskLevel x y =  (Array2D.get grid y x) + 1
      grid |> getLowPoints |>  Seq.sumBy (fun (x,y) -> riskLevel x y)
    

[<Fact>]
let ``Part 1 sample`` () =
    sampleGrid|> evaluateRiskLevel |> should equal 15
    
[<Fact>]
let ``Day 9 Part 1`` () =
    let text = IO.File.ReadAllText "day9Input.txt"
    let grid = parseGrid text
    grid|> evaluateRiskLevel |> should equal 572

let rec sizeBassin grid (x,y)  = 
        let rec sizeBassinRec alreadyCounted (x,y) =
            let currentLevel = Array2D.get grid y x    
            let isHigherAndNot9 (x,y) =
                let level = Array2D.get grid y x
                level > currentLevel && level < 9
            let notYet (x,y) = Set.contains (x,y) alreadyCounted |> not
            let filter (x,y) = isHigherAndNot9 (x,y) && notYet (x,y)
            let upStream = neighbors grid x y |> Seq.filter filter
            let alreadyCounted = Set.add (x,y) alreadyCounted
            Seq.fold sizeBassinRec alreadyCounted upStream
        Set.count (sizeBassinRec Set.empty (x,y))
            
let multiply3largestBassinsSize grid =
    grid |> getLowPoints |> Seq.map (sizeBassin grid) |> Seq.sortDescending |> Seq.take 3 |> Seq.reduce (*)


[<Fact>]
let ``First bassin`` () =
    sizeBassin sampleGrid (1,0) |> should equal 3

[<Fact>]
let ``Second bassin`` () =
    sizeBassin sampleGrid (9,0) |> should equal 9
    
[<Fact>]
let ``Third bassin`` () =
    sizeBassin sampleGrid (2,2) |> should equal 14


[<Fact>]
let ``Part 2 sample`` () =
    let grid = parseGrid sample
    grid|> multiply3largestBassinsSize |> should equal 1134
    
    
[<Fact>]
let ``Day 9 Part 2`` () =
    let text = IO.File.ReadAllText "day9Input.txt"
    let grid = parseGrid text
    grid|> multiply3largestBassinsSize |> should equal 847044
