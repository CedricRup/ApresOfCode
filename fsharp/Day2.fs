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

