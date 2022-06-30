module Day11

open FSharpx
open FSharpx.Text
open Xunit
open FsUnit.Xunit


type Octopus = int
type Cavern = Octopus[,]

let step stepCount cavern : int =
    cavern
    |> Array2D.map (fun octupusEnergy ->  (octupusEnergy + stepCount) / 10) 
    |>  Seq.cast<Octopus> |> Seq.sum
     
    
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
let ``A couple of unsynchronized octupusses`` () =
    let cavern = array2D [[9;8]]
    cavern |> step 1 |> should equal 2
   