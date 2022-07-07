module Day11

open FSharpx
open FSharpx.Text
open Xunit
open FsUnit.Xunit


type Octopus = int
type Cavern = Octopus[,]

let nextStep cavern =
    let energizeOctupus octupusEnergy =
        (octupusEnergy + 1) % 10
    cavern |> Array2D.map energizeOctupus

let step stepCount (cavern: Cavern) : int =
    let energizeOctupus octupusEnergy =
        octupusEnergy + 1
    let folder (cavern : Cavern) _ = 
        cavern
        |> nextStep
   
    [1..stepCount]
    |> List.scan folder cavern
    |> List.map (fun c -> c |> Seq.cast<Octopus> |> Seq.filter (fun o -> o = 0) |> Seq.length)
    |> Seq.sum
    
[<Fact>]
let ``Lonely octupus with low energy doesn't flash in one step`` () =
    let cavern = array2D [[0]]
    cavern |> step 1 |> should equal 0
    
[<Fact>]
let ``Lonely octupus with low energy flashes with enough steps`` () =
    let cavern = array2D [[0]]
    cavern |> step 10 |> should equal 1
    
[<Fact>]
let ``Lonely octupus with 5 energy flashes with 5 steps`` () =
    let cavern = array2D [[5]]
    cavern |> step 5 |> should equal 1
    
[<Fact>]
let ``Lonely octupus with 5 energy flashes twice with 15 steps`` () =
    let cavern = array2D [[5]]
    cavern |> step 15 |> should equal 2
    
    
[<Fact>]
let ``A couple of synchronized octupusses`` () =
    let cavern = array2D [[9;9]]
    cavern |> step 1 |> should equal 2


[<Fact>]
let ``Lonely octopus reset energy after flash`` () =
    let cavern = array2D [[9]]
    cavern |> nextStep |> Seq.cast<Octopus> |> Seq.head |> should equal 0
    
//[<Fact>]
let ``A couple of unsynchronized octupusses`` () =
    let cavern = array2D [[9;8]]
    cavern |> step 1 |> should equal 2
   