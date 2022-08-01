module Day12

open FSharpx.Collections
open Xunit
open FsUnit.Xunit
open FSharpx.String
open System


type Cave =
    | End
    | BigCave of string
    | SmallCave of string
    
let countPathes input =
    let parseCave cave =
        match cave with
        | "end" -> End
        | x when x.ToUpper() = x -> BigCave x
        | _ -> SmallCave cave
    let parseEdge edgeText =
        let edges = splitString [|"-"|] StringSplitOptions.TrimEntries edgeText
        (parseCave edges[0], parseCave edges[1])
    let containsCave cavern edge  = fst edge = cavern || snd edge = cavern
    
    let edges = splitString [|"\n"|] StringSplitOptions.RemoveEmptyEntries input |> Array.map parseEdge
    
    let rec countPathes' start edges  =
        let starts = edges |> Array.filter (containsCave start)
        let countPathesFolowingEdge remainingEdges (one, two)  =
                let newStart = if one = start then two else one
                countPathes' newStart remainingEdges
        match start with
        | End -> 1
        | SmallCave _ ->
            let nonStarts = edges |> Array.filter (containsCave start >> not)
            starts |> Array.sumBy (countPathesFolowingEdge nonStarts)
        | BigCave _ ->
            starts |> Array.sumBy (countPathesFolowingEdge edges)
    countPathes' (SmallCave "start") edges
                
      
    
    
[<Fact>]
let ``just start and end gives 1``() =
    let input = "start-end"
    countPathes input |> should equal 1

[<Fact>]
let ``Straight path with 1 small cavern gives 1``() =
    let input = """start-a
a-end"""
    countPathes input |> should equal 1
    
[<Fact>]
let ``Straight path with 1 small cavern and shortcut gives 2``() =
    let input = """start-a
a-end
start-end"""
    countPathes input |> should equal 2
    
[<Fact>]
let ``2 paths but one exit gives 2``() =
    let input = """start-a
a-end
start-b
b-a"""
    countPathes input |> should equal 2

[<Fact>]
let ``sample``() =
    let input = """start-A
start-b
A-c
A-b
b-d
A-end
b-end"""
    countPathes input |> should equal 10

[<Fact>]
let ``larger sample``() =
    let input = """fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"""
    countPathes input |> should equal 226

[<Fact>]
let ``day 12 part 1``() =
    let input = System.IO.File.ReadAllText "day12input.txt"
    countPathes input |> should equal 3292