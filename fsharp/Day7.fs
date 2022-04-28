module Day7

open Xunit
open FsUnit.Xunit


let calculateMinimumFuelForAlignement fuelConsumptionStrategy positions =
    let minimum = List.min positions
    let maximum = List.max positions
    let allCrabs crabs position = List.sumBy (fuelConsumptionStrategy position) crabs
    [minimum..maximum] |> List.map  (allCrabs positions) |> List.min

let naiveConsumption destination initial  = abs (initial - destination)
let naiveCrabAlignment = calculateMinimumFuelForAlignement naiveConsumption   
     
[<Fact>]
let ``From example``() =
    [16;1;2;0;4;2;7;1;2;14] |> naiveCrabAlignment |> should equal 37
    
[<Fact>]
let ``Day 7 part 1``() =
    System.IO.File.ReadAllText "day7Input.txt" |> FSharpx.String.splitChar [|','|]
        |> Array.toList
        |> List.map int
        |> naiveCrabAlignment |> should equal 348664

let realConsumption destination initial =
    let n = abs (destination - initial)
    n * (n+1) / 2
    
let realCrabAlignment = calculateMinimumFuelForAlignement realConsumption

[<Fact>]
let ``Real consumption from example``() =
    [16;1;2;0;4;2;7;1;2;14] |> realCrabAlignment |> should equal 168
    
[<Fact>]
let ``Day 7 part 2``() =
    System.IO.File.ReadAllText "day7Input.txt" |> FSharpx.String.splitChar [|','|]
        |> Array.toList
        |> List.map int
        |> realCrabAlignment |> should equal 100220525