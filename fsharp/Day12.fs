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
    
type Status =
    | CanVisitACaveTwice
    | AlreadyVisited of Cave
    | CannotRevisitAnyCave

let parseInput input =
    let parseCave cave =
        match cave with
        | "end" -> End
        | x when x.ToUpper() = x -> BigCave x
        | _ -> SmallCave cave
    let parseEdge edgeText =
        let edges = splitString [|"-"|] StringSplitOptions.TrimEntries edgeText
        (parseCave edges[0], parseCave edges[1])
    
    splitString [|"\n"|] StringSplitOptions.RemoveEmptyEntries input |> Array.map parseEdge

let containsCave cavern edge  = fst edge = cavern || snd edge = cavern

let countPathes status input =
    
    let rec countPathes' status path origin edges : string array  =
        
        let countPathesFolowingEdge (status:Status) path remainingEdges (one, two)  =
                let newStart = if one = origin then two else one
                countPathes' status path newStart remainingEdges
        
        let edgesToFollow,edgesWithouOrigin = edges |> Array.partition (containsCave origin)
        
        match origin with
        | End -> [|path + "-end"|]
        | SmallCave "start" ->
            let path = "start"
            edgesToFollow |> Array.collect (countPathesFolowingEdge status path edgesWithouOrigin)
        | SmallCave x ->
            let path = path + "-" + x
            match status with
            | AlreadyVisited cave when cave = origin ->
                edgesToFollow |> Array.collect (countPathesFolowingEdge CannotRevisitAnyCave path edgesWithouOrigin)
            | CannotRevisitAnyCave | AlreadyVisited _ ->
                edgesToFollow |> Array.collect (countPathesFolowingEdge status path edgesWithouOrigin)
            | CanVisitACaveTwice ->
                let firstWay = edgesToFollow |> Array.collect (countPathesFolowingEdge (AlreadyVisited origin) path edges)
                let otherWay = edgesToFollow |> Array.collect (countPathesFolowingEdge CanVisitACaveTwice path edgesWithouOrigin)
                Array.append firstWay otherWay
        | BigCave x ->
            let path = path + "-" + x
            edgesToFollow |> Array.collect (countPathesFolowingEdge status path edges)
    parseInput input |> countPathes' status "" (SmallCave "start") |> Set.ofArray |> Set.count     

let countPathesMaybeVisitingASmallCaveTwice  =
    countPathes CanVisitACaveTwice 
        
    
let countPathesVisitingSmallCavesOnce =
    countPathes CannotRevisitAnyCave 
    
    
[<Fact>]
let ``just start and end gives 1``() =
    let input = "start-end"
    countPathesVisitingSmallCavesOnce input |> should equal 1

[<Fact>]
let ``Straight path with 1 small cavern gives 1``() =
    let input = """start-a
a-end"""
    countPathesVisitingSmallCavesOnce input |> should equal 1
    
[<Fact>]
let ``Straight path with 1 small cavern and shortcut gives 2``() =
    let input = """start-a
a-end
start-end"""
    countPathesVisitingSmallCavesOnce input |> should equal 2
    
[<Fact>]
let ``2 paths but one exit gives 2``() =
    let input = """start-a
a-end
start-b
b-a"""
    countPathesVisitingSmallCavesOnce input |> should equal 2


let theSample = """start-A
start-b
A-c
A-b
b-d
A-end
b-end"""

[<Fact>]
let ``sample for part 1``() =
    countPathesVisitingSmallCavesOnce theSample |> should equal 10

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
    countPathesVisitingSmallCavesOnce input |> should equal 226

[<Fact>]
let ``day 12 part 1``() =
    let input = System.IO.File.ReadAllText "day12input.txt"
    countPathesVisitingSmallCavesOnce input |> should equal 3292
    
[<Fact>]
let ``The sample for part 2``() =
    countPathesMaybeVisitingASmallCaveTwice theSample |> should equal 36
    
[<Fact>]
let ``beware not to count same path twice``() =
    let input = """start-b
b-d
b-end"""
    countPathesMaybeVisitingASmallCaveTwice input |> should equal 2
    
[<Fact>]
let ``day 12 part 2``() =
    let input = System.IO.File.ReadAllText "day12input.txt"
    countPathesMaybeVisitingASmallCaveTwice input |> should equal 89592
