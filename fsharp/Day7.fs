module Day7

open Xunit
open FsUnit.Xunit
open FParsec
open fsharp.Common


let sampleInput = [16;1;2;0;4;2;7;1;2;14]

let alignWithBadCrabEngineering crabPositions destination =
    List.sumBy (fun p -> abs (p - destination)) crabPositions
    
let alignWithCorrectCrabEngineering crabPositions destination =
    let alignCrab initial  =
        let n = abs(initial - destination)
        (n * (n + 1))/2
    List.sumBy alignCrab crabPositions
    

let minimumConsumptionForAlignmentUsingMethod method crabPositions  =
    let minimumPostion = List.min crabPositions
    let maximumPostion = List.max crabPositions
    [minimumPostion..maximumPostion] |> List.map (method crabPositions) |> List.min

let minimumConsumptionWithBadCrabEngineering =
    minimumConsumptionForAlignmentUsingMethod alignWithBadCrabEngineering

let minimumConsumptionForAlignmentWithCorrectCrabEngineering =
    minimumConsumptionForAlignmentUsingMethod alignWithCorrectCrabEngineering
    
[<Fact>]
let ``Can calculate fuel consumption for 1 crab``() =
    alignWithBadCrabEngineering sampleInput 1 |> should equal 41
    
[<Fact>]
let ``Can calculate minimal full consumption for alignement``() =
    minimumConsumptionWithBadCrabEngineering sampleInput |> should equal 37

[<Fact>]
let ``Day 7 part 1`` () =
        System.IO.File.ReadAllText "day7Input.txt"
        |> run intSeparatedByColumnParser
        |> resultOrDie
        |> minimumConsumptionWithBadCrabEngineering
        |> should equal 348664

[<Fact>]
let ``Align with crab Engineering``() =
    alignWithCorrectCrabEngineering sampleInput 5 |> should equal 168

[<Fact>]
let ``Can calculate minimal full consumption for alignement with correct crab engineering``() =
    minimumConsumptionForAlignmentWithCorrectCrabEngineering sampleInput |> should equal 168

[<Fact>]
let ``Day 7 part 2`` () =
        System.IO.File.ReadAllText "day7Input.txt"
        |> run intSeparatedByColumnParser
        |> resultOrDie
        |> minimumConsumptionForAlignmentWithCorrectCrabEngineering
        |> should equal 100220525
