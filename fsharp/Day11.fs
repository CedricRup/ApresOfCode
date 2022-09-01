module Day11

open FSharpx
open FSharpx.Text
open Xunit
open FsUnit.Xunit


type Octopus =
    | Energy of int
    | Flashed
type Cavern = Octopus[,]

let nextStep cavern =
    let energizeOctupus octupusEnergy =
        match octupusEnergy with
        | Flashed -> Flashed
        | Energy i -> if i = 9 then Flashed else i + 1 |> Energy
    cavern |> Array2D.map energizeOctupus |> Array2D.map (function Flashed -> Energy 0 | o -> o )

let step stepCount (cavern: Cavern) : int =
    let folder (cavern : Cavern) _ = 
        cavern
        |> nextStep
        
   
    [1..stepCount]
    |> List.scan folder cavern
    |> List.skip 1
    |> List.map (fun c -> c |> Seq.cast<Octopus> |> Seq.filter (fun o -> o = Energy 0) |> Seq.length)
    |> Seq.sum
    
[<Fact>]
let ``Lonely octupus with low energy doesn't flash in one step`` () =
    let cavern = array2D [[0]] |> Array2D.map Energy
    cavern |> step 1 |> should equal 0
    
[<Fact>]
let ``Lonely octupus with low energy flashes with enough steps`` () =
    let cavern = array2D [[0]] |> Array2D.map Energy
    cavern |> step 10 |> should equal 1
    
[<Fact>]
let ``Lonely octupus with 5 energy flashes with 5 steps`` () =
    let cavern = array2D [[5]] |> Array2D.map Energy
    cavern |> step 5 |> should equal 1
    
[<Fact>]
let ``Lonely octupus with 5 energy flashes twice with 15 steps`` () =
    let cavern = array2D [[5]] |> Array2D.map Energy
    cavern |> step 15 |> should equal 2
    
    
[<Fact>]
let ``A couple of synchronized octupusses`` () =
    let cavern = array2D [[9;9]] |> Array2D.map Energy
    cavern |> step 1 |> should equal 2


[<Fact>]
let ``Lonely octopus reset energy after flash`` () =
    let cavern = array2D [[9]] |> Array2D.map Energy
    cavern |> nextStep |> Seq.cast<Octopus> |> Seq.head |> should equal (Energy 0)
    
//[<Fact>]
let ``A couple of unsynchronized octupusses`` () =
    let cavern = array2D [[9;8]] |> Array2D.map Energy
    cavern |> step 1 |> should equal 2
   