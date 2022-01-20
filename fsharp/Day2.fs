module Day2

open System
open Xunit

type Position = {
    Horizontal:int
    Depth:int
}

type Command =
    | Forward of int
    | Down of int
    | Up of int

let move initialPosition command =
    match (initialPosition,command) with
    | ({Horizontal =  h}, Forward x) -> {initialPosition with Horizontal = h + x}
    | ({Depth =  d}, Down x) -> {initialPosition with Depth = d + x}
    | ({Depth =  d}, Up x) when x > d -> {initialPosition with Depth = 0}
    | ({Depth =  d}, Up x) -> {initialPosition with Depth = d - x}

let followPlan plan =
    List.fold move {Horizontal=0;Depth=0} plan

let toCommand (input:String) =
    let splited = input.Split()
    let direction =
        match splited[0] with
        |   "forward" -> Forward
        |    "up" -> Up
        |   "down" -> Down
        | _ -> failwith "Unknow command"
    splited[1] |> int |> direction

[<Fact>]
let ``Forward adds value to Horizontal position`` () =
    let newPosition =  move {Horizontal= 0; Depth=0} (Forward 1)
    Assert.Equal({Horizontal = 1;Depth=0},newPosition)

[<Fact>]
let ``Down adds value to Depth`` () =
    let newPosition =  move {Horizontal= 0; Depth=0} (Down 1)
    Assert.Equal({Horizontal = 0;Depth=1},newPosition)

[<Fact>]
let ``Up substacts value to Depth`` () =
    let newPosition =  move {Horizontal= 0; Depth=1} (Up 1)
    Assert.Equal({Horizontal = 0;Depth=0},newPosition)

[<Fact>]
let ``A submarine doesn't fly`` () =
    let newPosition =  move {Horizontal= 0; Depth=0} (Up 1)
    Assert.Equal({Horizontal = 0;Depth=0},newPosition)

[<Fact>]
let ``A submarine really doesn't fly`` () =
    let newPosition =  move {Horizontal= 0; Depth=3} (Up 5)
    Assert.Equal({Horizontal = 0;Depth=0},newPosition)

[<Fact>]
let ``A submarine follows a plan`` () = 
    let plan = [
        Forward 5
        Down 5
        Forward 8
        Up 3
        Down 8
        Forward 2
    ]
    let newPosition = followPlan plan 
    Assert.Equal({Horizontal = 15;Depth=10},newPosition)

[<Fact>]
let ``Can translate Forward`` () = 
    let input = "forward 3"
    let result = toCommand input
    Assert.Equal(Forward 3, result)

[<Fact>]
let ``Can translate Up`` () = 
    let input = "up 3"
    let result = toCommand input
    Assert.Equal(Up 3, result)

[<Fact>]
let ``Can translate Down`` () = 
    let input = "down 3"
    let result = toCommand input
    Assert.Equal(Down 3, result)

[<Fact>]
let ``Day two Part One`` () =
    let plan = 
        IO.File.ReadAllLines "day2Input.txt"
        |> Array.toList
        |> List.map toCommand
    let position  = followPlan plan
    Assert.Equal({Horizontal = 1957; Depth=955},position)



type Vector = {Position : Position;Aim : int} 

let moveWithAim vector command =
    let {Position = {Horizontal = h;Depth = d}; Aim = a}  = vector
    match command with
    | Forward x  ->
         {vector with Position =  {Horizontal = h + x; Depth = Math.Max(d + a * x, 0) }}
    | Down x -> {vector with Aim = a+x}
    | Up x -> {vector with Aim = a-x}

let followPlanWithAim plan =
    List.fold moveWithAim {Position = {Horizontal=0;Depth=0}; Aim = 0} plan


[<Fact>]
let ``Forward x moves horizontally by x if aim is 0`` () =
    let newVector =  moveWithAim {Position = {Horizontal= 0; Depth=0}; Aim=0} (Forward 5)
    Assert.Equal({Position = {Horizontal = 5;Depth=0}; Aim=0},newVector)

[<Fact>]
let ``Down x adds x to aim`` () =
    let newVector =  moveWithAim {Position = {Horizontal = 5;Depth=0}; Aim=0} (Down 5)
    Assert.Equal({Position = {Horizontal = 5;Depth=0}; Aim=5},newVector)

[<Fact>]
let ``Forward x move by x horizontally and by aim * x in depth`` () =
    let newVector =  moveWithAim {Position = {Horizontal = 5;Depth=0}; Aim=5} (Forward 8)
    Assert.Equal({Position = {Horizontal = 13;Depth=40}; Aim=5},newVector)

[<Fact>]
let ``Up x substracts x to aim`` () =
    let newVector =  moveWithAim {Position = {Horizontal = 13;Depth=40}; Aim=5} (Up 3)
    Assert.Equal({Position = {Horizontal = 13;Depth=40}; Aim=2},newVector)

[<Fact>]
let ``Forward decrease depth if aim is negative`` () =
    let newVector =  moveWithAim {Position = {Horizontal = 0;Depth=5}; Aim=(-3)} (Forward 1)
    Assert.Equal({Position = {Horizontal = 1;Depth=2}; Aim=(-3)},newVector)

[<Fact>]
let ``A submarine still can't fly`` () =
    let newVector =  moveWithAim {Position = {Horizontal = 0;Depth=2}; Aim=(-3)} (Forward 1)
    Assert.Equal({Position = {Horizontal = 1;Depth=0}; Aim=(-3)},newVector)

[<Fact>]
let ``Day two Part Two`` () =
    let plan = 
        IO.File.ReadAllLines "day2Input.txt"
        |> Array.toList
        |> List.map toCommand
    let vector  = followPlanWithAim plan
    Assert.Equal({Horizontal = 1957; Depth=1004584},vector.Position)
