module Day7

open Xunit
open FsUnit.Xunit
open FParsec
open fsharp.Common


let sampleInput = [16;1;2;0;4;2;7;1;2;14]

let align crabPositions destination =
    List.sumBy (fun p -> abs (p - destination)) crabPositions 

let minimumConsumptionForAlignment crabPositions =
    let minimumPostion = List.min crabPositions
    let maximumPostion = List.max crabPositions
    [minimumPostion..maximumPostion] |> List.map (align crabPositions) |> List.min
   
[<Fact>]
let ``Can calculate fuel consumption for 1 crab``() =
    align sampleInput 1 |> should equal 41
    
[<Fact>]
let ``Can calculate minimal full consumption for alignement``() =
    minimumConsumptionForAlignment sampleInput |> should equal 37

[<Fact>]
let ``Day 7 part 1`` () =
        System.IO.File.ReadAllText "day7Input.txt"
        |> run intSeparatedByColumnParser
        |> resultOrDie
        |> minimumConsumptionForAlignment
        |> should equal 348664
