module Day6

open Xunit
open FsUnit.Xunit

let countFishAfterDays numberOfDays initialState  =
    let BigIntCountBy list = List. countBy id list |> List.map (fun (key,value) -> (key, int64 value))
    let initialSchool = BigIntCountBy initialState |> Map.ofList
    let addToSchool toAdd alreadyThere =
        match alreadyThere with
        | None -> Some toAdd
        | Some number -> Some (toAdd+number)
    let bringSchoolToNextGen newSchools key value =
        match key with
        | 0 -> Map.change 6 (addToSchool value) newSchools |> Map.add 8 value
        | 7 -> Map.change 6 (addToSchool value) newSchools
        | e -> Map.add (e-1) value newSchools
    let countFishesInAllSchools = Map.fold (fun sum _ number -> sum+number) 0L
    let bringAllSchoolsToNextGen state _  = Map.fold bringSchoolToNextGen Map.empty state
    [1..numberOfDays] |> List.fold bringAllSchoolsToNextGen initialSchool |> countFishesInAllSchools
    
[<Fact>]
let ``Can count fish for 1 generation``() =
    [2;3;2;0;1] |> countFishAfterDays 1 |> should equal 6L
    
[<Fact>]
let ``Can count fish for 2nd generation``() =
    [2;3;2;0;1] |> countFishAfterDays 2 |> should equal 7L
    
[<Fact>]
let ``Can count fish for 80nd generation``() =
    [3;4;3;1;2] |> countFishAfterDays 80 |> should equal 5934L


[<Fact>]
let ``Day 6 part 1``() =
    System.IO.File.ReadAllText "day6Input.txt" |> FSharpx.String.splitChar [|','|]
        |> Array.toList
        |> List.map int
        |> countFishAfterDays 80 |> should equal 393019L
    
[<Fact>]
let ``Day 6 part 2``() =
    System.IO.File.ReadAllText "day6Input.txt" |> FSharpx.String.splitChar [|','|]
        |> Array.toList
        |> List.map int
        |> countFishAfterDays 256 |> should equal 1757714216975L
