module Day1

open System
open Xunit



let depthIncreaseCount depthMeasurements = 
    List.windowed 2 depthMeasurements |>
    List.sumBy(function 
    | [a;b] when a < b -> 1
    | _ -> 0)

[<Fact>]
let ``A decrease doesn't count`` () =
    let depthMeasurements = [210;200]
    let increaseCount = depthIncreaseCount depthMeasurements
    Assert.Equal(0,increaseCount)

[<Fact>]
let ``An increase counts`` () =
    let depthMeasurements = [199;200]
    let increaseCount = depthIncreaseCount depthMeasurements
    Assert.Equal(1,increaseCount)

[<Fact>]
let ``An equality doesn't count`` () =
    let depthMeasurements = [199;199]
    let increaseCount = depthIncreaseCount depthMeasurements
    Assert.Equal(0,increaseCount)

[<Fact>]
let ``We count all increases`` () =
    let depthMeasurements = [199 ;200 ;208 ;210 ;200 ;207 ;240 ;269 ;260 ;263 ;]
    let increaseCount = depthIncreaseCount depthMeasurements
    Assert.Equal(7,increaseCount)

[<Fact>]
let ``Day one Part One`` () =
    let depthMeasurements = 
        IO.File.ReadAllLines "day1Input.txt"
        |> Array.toList
        |> List.map int
    let increaseCount = depthIncreaseCount depthMeasurements
    Assert.Equal(1446,increaseCount)


let sumOf3ElementSlidingWindow depthMeasurements =
    List.windowed 3 depthMeasurements |>
    List.map List.sum

let threeElementSlidingWindowIncreaseCount = 
    sumOf3ElementSlidingWindow >> depthIncreaseCount


[<Fact>]
let ``?`` () =
    let sample = [1;2;3]
    let result = sumOf3ElementSlidingWindow sample
    Assert.Equal<list<int>>([6],result)

[<Fact>]
let ``??`` () =
    let sample = [1;2;3;4]
    let result = sumOf3ElementSlidingWindow sample
    Assert.Equal<list<int>>([6;9],result)

[<Fact>]
let ``???`` () =
    let sample = [1;2]
    let result = sumOf3ElementSlidingWindow sample
    Assert.Equal<list<int>>([],result)

[<Fact>]
let ``????`` () =
    let sample = [
        199       
        200 
        208 
        210 
        200 
        207 
        240 
        269 
        260 
        263
    ] 
    let result = threeElementSlidingWindowIncreaseCount sample
    Assert.Equal(5,result)

[<Fact>]
let ``Day one Part Two`` () =
    let depthMeasurements = 
        IO.File.ReadAllLines "day1Input.txt"
        |> Array.toList
        |> List.map int
    let increaseCount = threeElementSlidingWindowIncreaseCount depthMeasurements
    Assert.Equal(1486,increaseCount)



