module Day6

open System
open Xunit
open FsUnit.Xunit
open FSharpx.String

type Fish = Fish of int

let naiveTimeFliesBy =
    function
    | Fish 0 -> [(Fish 6);(Fish 8)]
    | Fish n -> [Fish (n-1)]

let naiveCountFish day fish =
    let folder fishes _ = List.collect naiveTimeFliesBy fishes
    [1..day] |>
    List.fold folder fish
    |> List.length
    
    
type SchoolOfFish = {
    timeToReproduction : int;
    count : int64
}

let timeFliesBy school =
    match school with
    | {timeToReproduction = 0; count = count} -> [{timeToReproduction = 6;count = count};{timeToReproduction=8;count=count}]
    | {timeToReproduction = x; count = count} -> [{timeToReproduction = x-1;count = count}]

let countFish day fishes =
    let timeToReproductionForFish = function |Fish n -> n
    let timeToReproductionForSchool school = school.timeToReproduction
    let countOfSchool school = school.count
    
    let toSchool (timeToReprodction,count) = {timeToReproduction=timeToReprodction;count = int64 count}
    let toSchools fishes =
        List.countBy timeToReproductionForFish fishes
        |> List.map toSchool
    
    let regroupSchoolsByReproductionTime schools =
        List.groupBy timeToReproductionForSchool schools
        |> List.map (fun (k,s) -> {timeToReproduction = k;count = List.sumBy countOfSchool s })
    
    let folder schools _ =
        List.collect timeFliesBy schools
        |> regroupSchoolsByReproductionTime
    
    let initialSchool = toSchools fishes   
    List.fold folder initialSchool [1..day] |> List.sumBy countOfSchool
    
[<Fact>]
let ``A fish at 6 gives a fish at 5``() =
    naiveTimeFliesBy (Fish 6) |> should equal [(Fish 5)]

[<Fact>]
let ``A fish at 5 gives a fish at 4``() =
    naiveTimeFliesBy (Fish 5) |> should equal [(Fish 4)]

[<Fact>]
let ``A fish at 0 gives a fish at 8 and go back to 6``() =
    naiveTimeFliesBy (Fish 0) |> should equal [(Fish 6);(Fish 8)]
    
[<Fact>]
let ``Part 1 example``() =
    let input = [3;4;3;1;2] |> List.map Fish
    naiveCountFish 80 input |> should equal 5934
    
[<Fact>]
let ``naive Part 1``() =
    System.IO.File.ReadAllText "day6Input.txt"
    |> splitString [|","|] StringSplitOptions.RemoveEmptyEntries
    |> Array.toList
    |> List.map (int>>Fish)
    |> naiveCountFish 80
    |> should equal 393019
    
[<Fact>]
let ``Part 1``() =
    System.IO.File.ReadAllText "day6Input.txt"
    |> splitString [|","|] StringSplitOptions.RemoveEmptyEntries
    |> Array.toList
    |> List.map (int>>Fish)
    |> countFish 80
    |> should equal 393019L
    
[<Fact>]
let ``Part 2``() =
    System.IO.File.ReadAllText "day6Input.txt"
    |> splitString [|","|] StringSplitOptions.RemoveEmptyEntries
    |> Array.toList
    |> List.map (int>>Fish)
    |> countFish 256
    |> should equal 1757714216975L
    
    