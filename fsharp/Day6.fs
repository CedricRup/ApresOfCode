module Day6

open System
open Xunit
open FsUnit.Xunit
open FSharpx.String

type Fish = Fish of int

let timeFliesBy =
    function
    | Fish 0 -> [(Fish 6);(Fish 8)]
    | Fish n -> [Fish (n-1)]

let countFish day fish =
    let folder fishes _ = List.collect timeFliesBy fishes
    [1..day] |>
    List.fold folder fish
    |> List.length

[<Fact>]
let ``A fish at 6 gives a fish at 5``() =
    timeFliesBy (Fish 6) |> should equal [(Fish 5)]

[<Fact>]
let ``A fish at 5 gives a fish at 4``() =
    timeFliesBy (Fish 5) |> should equal [(Fish 4)]

[<Fact>]
let ``A fish at 0 gives a fish at 8 and go back to 6``() =
    timeFliesBy (Fish 0) |> should equal [(Fish 6);(Fish 8)]
    
[<Fact>]
let ``Part 1 example``() =
    let input = [3;4;3;1;2] |> List.map Fish
    countFish 80 input |> should equal 5934
    
[<Fact>]
let ``Part 1``() =
    System.IO.File.ReadAllText "day6Input.txt"
    |> splitString [|","|] StringSplitOptions.RemoveEmptyEntries
    |> Array.toList
    |> List.map (int>>Fish)
    |> countFish 80
    |> should equal 393019