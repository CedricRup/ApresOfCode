module Day7

open System
open Xunit
open FsUnit.Xunit
open FSharpx.String


let calculateMinimumFuelForAlignement positions =
    let minimum = List.min positions
    let maximum = List.max positions
    let crabToPostion destination initial  = abs (initial - destination)
    let allCrabs crabs position = List.sumBy (crabToPostion position) crabs
    [minimum..maximum] |> List.map  (allCrabs positions) |> List.min
    
     
[<Fact>]
let ``From example``() =
    [16;1;2;0;4;2;7;1;2;14] |> calculateMinimumFuelForAlignement |> should equal 37
    
[<Fact>]
let ``Day 7 part 1``() =
    System.IO.File.ReadAllText "day7Input.txt" |> FSharpx.String.splitChar [|','|]
        |> Array.toList
        |> List.map int
        |> calculateMinimumFuelForAlignement |> should equal 348664
