module Day11

open Xunit
open FsUnit.Xunit
open FSharpx.String
open System


type Octopus =  | Flashed
                | Energized of int
type Cavern = Octopus[,]

let private resetOctopus = function
        | Flashed -> Energized 0
        | x -> x
    
let generateEveryStep stepCount cavern =
    let cavernXSize = Array2D.length2 cavern
    let cavernYSize = Array2D.length1 cavern
    let isInbounds (x,y) = x >= 0 && x < cavernXSize && y >= 0 && y < cavernYSize
    
    let rec energizeOctupus (cavern: Cavern) (x,y)  =
        let octopus = (Array2D.get cavern y x)
        match octopus with
        | Flashed -> cavern
        | Energized energy ->
            let newEnergy = energy + 1
            let octopus = if newEnergy = 10 then Flashed else Energized newEnergy
            Array2D.set cavern y x octopus    
            if (octopus = Flashed) then
                [(x-1,y-1);(x,y-1);(x+1,y-1);(x-1,y);(x+1,y);(x-1,y+1);(x,y+1);(x+1,y+1)]
                |> List.filter isInbounds
                |> List.fold energizeOctupus cavern
            else cavern     
       
    let step cavern _ =
        let resetedCavern = Array2D.map resetOctopus cavern
        [
        for y in 0.. cavernYSize - 1 do
        for x in 0.. cavernXSize - 1 do
        yield (x,y)
        ] |> List.fold  energizeOctupus resetedCavern

    Seq.init stepCount id
    |> Seq.scan step cavern

let countFlashes stepCount cavern : int =
    generateEveryStep stepCount cavern
    |> Seq.collect Seq.cast<Octopus>
    |> Seq.sumBy (function |Flashed -> 1 | _ -> 0)

let allIgnited cavern =
    cavern
    |> Seq.cast<Octopus>
    |> Seq.forall (function | Flashed -> true | _ -> false)

let firstSync cavern =
    generateEveryStep Int32.MaxValue cavern |> Seq.takeWhile (fun c -> allIgnited c |> not) |> Seq.length
    
let splitLines = splitString  [|"\n"|] StringSplitOptions.RemoveEmptyEntries

let parse (input : string) =
    let test =
        input
        |> splitLines
        |> Array.map (toCharArray >> Array.map (string >> int >> Energized))
    
    test |> array2D  
     
let sampleCavern =
    "5483143223\n" +
    "2745854711\n" +
    "5264556173\n" +
    "6141336146\n" +
    "6357385478\n" +
    "4167524645\n" +
    "2176841721\n" +
    "6882881134\n" +
    "4846848554\n" +
    "5283751526\n"
    
[<Fact>]
let ``Lonely octupus with low energy doesn't flash in one step`` () =
    let cavern = array2D [[Energized 0]]
    cavern |> countFlashes 1 |> should equal 0
    
[<Fact>]
let ``Lonely octupus with low energy flashes with enough steps`` () =
    let cavern = array2D [[Energized 0]]
    cavern |> countFlashes 10 |> should equal 1
    
[<Fact>]
let ``Lonely octupus with 5 energy flashes with 5 steps`` () =
    let cavern = array2D [[Energized 5]]
    cavern |> countFlashes 5 |> should equal 1
    
[<Fact>]
let ``Lonely octupus with 5 energy flashes twice with 15 steps`` () =
    let cavern = array2D [[Energized 5]]
    cavern |> countFlashes 15 |> should equal 2
    
    
[<Fact>]
let ``A couple of synchronized octupusses`` () =
    let cavern = array2D [[Energized 9;Energized 9]]
    cavern |> countFlashes 1 |> should equal 2
    
[<Fact>]
let ``A couple of unsynchronized octupusses`` () =
    let cavern = array2D [[Energized 9;Energized 8]]
    cavern |> countFlashes 1 |> should equal 2
   
[<Fact>]
let ``A couple of unsynchronized octupusses but switching sides`` () =
    let cavern = array2D [[Energized 8;Energized 9]]
    cavern |> countFlashes 1 |> should equal 2
    
[<Fact>]
let ``3 horizontal unsynchronized octupusses`` () =
    let cavern = array2D [[Energized 8;Energized 9;Energized 8]]
    cavern |> countFlashes 1 |> should equal 3
    
[<Fact>]
let ``3 vertical unsynchronized octupusses`` () =
    let cavern = array2D [[Energized 8];[Energized 9];[Energized 8]]
    cavern |> countFlashes 1 |> should equal 3
    
[<Fact>]
let ``diagonal vertical unsynchronized octupusses`` () =
    let cavern = array2D [
        [Energized 8; Energized 0; Energized 8 ]
        [Energized 0; Energized 9; Energized 0]
        [Energized 8; Energized 0; Energized 8]
    ]
    cavern |> countFlashes 1 |> should equal 5


[<Fact>]
let ``Day 11 part 1 sample`` () =
    let cavern = parse sampleCavern
    cavern |> countFlashes 100 |> should equal 1656
    

[<Fact>]
let ``Day 11 part 1`` () =
    let text = IO.File.ReadAllText "day11Input.txt"
    let cavern = parse text
    cavern |> countFlashes 100 |> should equal 1721

[<Fact>]
let ``Day 11 part 2 sample`` () =
    let cavern = parse sampleCavern
    cavern |> firstSync |> should equal 195

[<Fact>]
let ``Day 11 part 2`` () =
    let text = IO.File.ReadAllText "day11Input.txt"
    let cavern = parse text
    cavern |> firstSync |> should equal 298
