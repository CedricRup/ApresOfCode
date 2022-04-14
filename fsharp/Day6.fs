module Day6

open Xunit
open FsUnit.Xunit

let countFishAfterDays numberOfDays initialState  =
    let initialSchool = List.countBy id initialState |> Map.ofList
    let addToSchool toAdd alreadyThere = 
        match alreadyThere with
        | None -> Some toAdd
        | Some number -> Some (toAdd+number)
    let bringSchoolToNextGen stateSchool key value =
        match key with
        | 0 -> Map.change 6 (addToSchool value) stateSchool |> Map.add 8 value
        | 7 -> Map.change 6 (addToSchool value) stateSchool
        | e -> Map.add (e-1) value stateSchool
    let countFishesInAllSchools = Map.fold (fun sum _ number -> sum+number) 0
    Map.fold bringSchoolToNextGen Map.empty initialSchool |> countFishesInAllSchools

[<Fact>]
let ``Can count fish for 1 generation``() =
    [2;3;2;0;1] |> countFishAfterDays 1 |> should equal 6
    
    
