module Day9

open System
open Xunit
open FsUnit.Xunit
open FSharpx.String
let sample =
    "2199943210\n" +
    "3987894921\n" +
    "9856789892\n" +
    "8767896789\n" +
    "9899965678\n" 

let splitLines = splitString  [|"\n"|] StringSplitOptions.RemoveEmptyEntries
let parse (input : string) =
    let test =
        input
        |> splitLines
        |> Array.map (toCharArray >> Array.map (string >> int))
    
    test |> array2D


let neighbors heightmap =
    let xMax = Array2D.length2 heightmap - 1
    let yMax = Array2D.length1 heightmap - 1
    
    fun (x,y) -> 
        let isInbounds (x,y) = x >= 0 && x <= xMax && y >= 0 && y <= yMax
        [(x-1,y);(x+1,y);(x,y-1);(x,y+1)] |> List.filter isInbounds
        

let getHeight heightmap (x,y)   = Array2D.get heightmap y x    
let findLowPoints heightmap  =
    let getHeight = getHeight heightmap
    let xMax = Array2D.length2 heightmap - 1
    let yMax = Array2D.length1 heightmap - 1
    let isLowPoint (x,y) =
        let currentHeight = getHeight (x,y) 
        neighbors heightmap (x,y)  |> List.map getHeight |> List.forall (fun height -> height > currentHeight)
    
    [
        for y in 0.. yMax do
        for x in 0.. xMax do
        if isLowPoint(x,y) then yield (x,y)
    ]
    
let getRiskLevels heightmap = findLowPoints heightmap |> List.map (getHeight heightmap )
        

[<Fact>]
let ``Can find low points in example`` () =
    sample |> parse |> getRiskLevels |> should equal  [1;0;5;5]

let riskLevel =
    List.sumBy (fun x -> x+1) 
    
[<Fact>]
let ``Can calculate total risk level for a heightmap`` () =
    sample |> parse |> getRiskLevels |> riskLevel |> should equal  15
    
[<Fact>]
let ``Day 9 Part 1`` () =
    let text = IO.File.ReadAllText "day9Input.txt"
    let grid = parse text
    grid|> getRiskLevels |> riskLevel |> should equal 572
    

let sizeBasin heightmap point =
    let getHeight = getHeight heightmap
    let neighbors = neighbors heightmap
    let rec sizeBasin alreadyCounted point  =
        let alreadyCounted = Set.add point alreadyCounted
        let referenceHeight = getHeight  point
        let isNotCountedYet point = Set.contains point alreadyCounted |> not
        let isInBasin point  =
            let height = getHeight point
            height <> 9 && height > referenceHeight
        let shouldBeConsideredNeighbor point = isInBasin point && isNotCountedYet point 
        let neighbors = neighbors point |> List.filter shouldBeConsideredNeighbor
        neighbors |> List.fold sizeBasin alreadyCounted
    sizeBasin Set.empty  point |> Set.count
    
[<Fact>]
let ``Basin with only lowpoint`` () =
    let input =
        "09\n" +
        "99"
    let heightmap = parse input
    sizeBasin heightmap (0,0) |> should equal 1
  
[<Fact>]
let ``Basin with size 2`` () =
    let input =
        "01\n" +
        "99"
    let heightmap = parse input
    sizeBasin heightmap (0,0) |> should equal 2

[<Fact>]
let ``Basin with size 3`` () =
    let input =
        "01\n" +
        "92"
    let heightmap = parse input
    sizeBasin heightmap (0,0) |> should equal 3
    
[<Fact>]
let ``suspicious bassin with possible duplicate`` () =
    let input =
        "10\n" +
        "21\n"
    let heightmap = parse input
    sizeBasin heightmap (1,0) |> should equal 4


let multiplySizeOf3LargestBasins heightmap =
    findLowPoints heightmap
    |> List.map (sizeBasin heightmap)
    |> List.sortDescending
    |> List.take 3 |> List.fold (*) 1 
    

[<Fact>]
let ``Part 2 example`` () =
    let heightmap = parse sample
    multiplySizeOf3LargestBasins heightmap |> should equal 1134

[<Fact>]
let ``Day 9 Part 2`` () =
    let text = IO.File.ReadAllText "day9Input.txt"
    let heightmap = parse text
    multiplySizeOf3LargestBasins heightmap |> should equal 847044
