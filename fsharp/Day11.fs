module Day11

open System
open Xunit
open FsUnit.Xunit
open FSharpx.String


type Octopus =
    | Energy of int
    | Flashed
type Cavern = Octopus[,]

let nextStep cavern =
    let dimX = Array2D.length2 cavern
    let dimY = Array2D.length1 cavern
    let addNeighbors x y toAppendTo =
        [
            (x-1, y-1);(x, y-1);(x+1, y-1)
            (x-1, y);(x+1, y)
            (x-1, y+1);(x, y+1);(x+1, y+1)
        ] |> List.filter (fun (x,y) -> x < dimX && x >=0 && y >=0 && y <dimY)  |> List.append toAppendTo
    let energizeFolder (cavern,toEnergize) (x,y) =
        let octopus = Array2D.get cavern y x
        match octopus with
            | Flashed -> cavern, toEnergize
            | Energy i ->
                 let octopus =  if i = 9 then Flashed else Energy (i + 1)
                 Array2D.set cavern y x octopus
                 if octopus = Flashed then (cavern,addNeighbors x y toEnergize) else cavern,toEnergize
        
    let rec energize' cavern toEnergize =
        match toEnergize with
        | [] -> cavern
        | _ ->
            let cavern,toEnergize = List.fold energizeFolder (cavern,[]) toEnergize
            energize' cavern toEnergize
    let cavern = cavern |> Array2D.map (function Flashed -> Energy 0 | o -> o )        
    let toEnergize = Array2D.mapi (fun  y x  _ -> (x,y)) cavern |> Seq.cast<int * int> |> Seq.toList
    energize' cavern toEnergize 
    
    

let countIndividualFlashesDuringSteps stepCount (cavern: Cavern) : int =
    let folder (cavern : Cavern) _ = 
        cavern
        |> nextStep
        
    let numberOfFlashedOctopus cavern =
        cavern
        |> Seq.cast<Octopus>
        |> Seq.filter (fun o -> o = Flashed)
        |> Seq.length
        
   
    [1..stepCount]
    |> List.scan folder cavern
    |> List.skip 1
    |> List.map numberOfFlashedOctopus
    |> Seq.sum
    
[<Fact>]
let ``Lonely octupus with low energy doesn't flash in one step`` () =
    let cavern = array2D [[0]] |> Array2D.map Energy
    cavern |> countIndividualFlashesDuringSteps 1 |> should equal 0
    
[<Fact>]
let ``Lonely octupus with low energy flashes with enough steps`` () =
    let cavern = array2D [[0]] |> Array2D.map Energy
    cavern |> countIndividualFlashesDuringSteps 10 |> should equal 1
    
[<Fact>]
let ``Lonely octupus with 5 energy flashes with 5 steps`` () =
    let cavern = array2D [[5]] |> Array2D.map Energy
    cavern |> countIndividualFlashesDuringSteps 5 |> should equal 1
    
[<Fact>]
let ``Lonely octupus with 5 energy flashes twice with 15 steps`` () =
    let cavern = array2D [[5]] |> Array2D.map Energy
    cavern |> countIndividualFlashesDuringSteps 15 |> should equal 2
    
    
[<Fact>]
let ``A couple of synchronized octupusses`` () =
    let cavern = array2D [[9;9]] |> Array2D.map Energy
    cavern |> countIndividualFlashesDuringSteps 1 |> should equal 2


[<Fact>]
let ``Lonely octopus reset energy after flash`` () =
    let cavern = array2D [[9]] |> Array2D.map Energy
    cavern |> nextStep |> Seq.cast<Octopus> |> Seq.head |> should equal Flashed
    
[<Fact>]
let ``A couple of unsynchronized octupusses`` () =
    let cavern = array2D [[9;8]] |> Array2D.map Energy
    cavern |> countIndividualFlashesDuringSteps 1 |> should equal 2
 
let example = @"5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"


let splitLines = splitString [|"\n"|] StringSplitOptions.RemoveEmptyEntries
let parse (input : string) = 
    input
    |> splitLines
    |> Array.map (toCharArray >> Array.map (string >> int >> Energy))
    |> array2D  

[<Fact>]
let ``Day 11 part 1 example `` () =
    let cavern = parse example
    cavern |> countIndividualFlashesDuringSteps 100 |> should equal 1656
    
[<Fact>]
let ``Day 11 part 1  `` () =
    let cavern = IO.File.ReadAllText "day11Input.txt" |>parse 
    cavern |> countIndividualFlashesDuringSteps 100 |> should equal 1721
    


let firstSyncFlash (cavern: Cavern) : int =
    let folder (cavern : Cavern, _) stepNumber = 
        (cavern |> nextStep,stepNumber)
    
    let AtLeastOneOctupusIsStillEnergized cavern = cavern |> Seq.cast<Octopus> |> Seq.exists (fun o -> o <> Flashed)
        
   
    seq {1..Int32.MaxValue}
    |> Seq.scan folder (cavern,0)
    |> Seq.skip 1
    |> Seq.skipWhile (fst >> AtLeastOneOctupusIsStillEnergized)
    |> Seq.head
    |> snd
    
  

[<Fact>]
let ``All 9 cavern has sync flash after 1 step`` () =
    let cavern = array2D [[9;9];[9;9]] |> Array2D.map Energy
    cavern |> firstSyncFlash  |> should equal 1
    
[<Fact>]
let ``Day 11 part 2 example `` () =
    let cavern = parse example
    cavern |> firstSyncFlash |> should equal 195
    
[<Fact>]
let ``Day 11 part 2  `` () =
    let cavern = IO.File.ReadAllText "day11Input.txt" |>parse 
    cavern |> firstSyncFlash |> should equal 298
    