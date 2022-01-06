module Tests

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
let ``?`` () =
    let depthMeasurements = [199 ;200 ;208 ;210 ;200 ;207 ;240 ;269 ;260 ;263 ;]
    let increaseCount = depthIncreaseCount depthMeasurements
    Assert.Equal(7,increaseCount)

[<Fact>]
let ``??`` () =
    let depthMeasurements = 
        IO.File.ReadAllLines "input.txt"
        |> Array.toList
        |> List.map int
    
     
    let increaseCount = depthIncreaseCount depthMeasurements
    Assert.Equal(1446,increaseCount)

