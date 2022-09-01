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
    let dimX = Array2D.length2 cavern
    let neighbors x y = [x+1, y] |> List.filter (fun (x,y) -> x < dimX) 
    let energizeOctupus toEnergize x y octupus  =
        if List.contains (x,y) toEnergize then  
            match octupus with
            | Flashed -> Flashed, []
            | Energy i -> if i = 9 then Flashed , (neighbors x y) else i + 1 |> Energy, []
        else octupus,[]
    let rec toto cavern toEnergize =
        match toEnergize with
        | [] -> cavern
        | _ ->
            let result = Array2D.mapi (energizeOctupus toEnergize) cavern
            let cavern = Array2D.map fst result
            let toEnergize = Array2D.map snd result |> Seq.cast<(int * int) list> |> Seq.concat |> Seq.toList
            toto cavern toEnergize
    let toEnergize = Array2D.mapi (fun  x y  _ -> (x,y)) cavern |> Seq.cast<int * int> |> Seq.toList
    toto cavern toEnergize |> Array2D.map (function Flashed -> Energy 0 | o -> o )

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
   