module Day3

open System
open Xunit


let sample = @"00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"

let calculateGamma (diagnosticReport: String list) =
    let numberOfSamples = List.length diagnosticReport
    
    let getColumnFromReport column  =
        diagnosticReport |> List.map (fun word -> word[column])

    let determineMostCommonBit list = 
        let sum = list |> List.sumBy (string>>int)
        if sum > numberOfSamples / 2 then 0b1 else 0b0  

    let wordLength = diagnosticReport.Head.Length

    [0.. wordLength-1] 
    |> List.map (getColumnFromReport >> determineMostCommonBit)
    |> List.reduce (fun word bit-> (word <<< 1) + bit)

let getEpsilon gamma =
    gamma |> (~~~) |> ((&&&) 0b011111) |>  int

[<Fact>]
let ``???`` () =
    let sample = [
        "1"
        "0"
        "1"
    ]    
    let result = calculateGamma sample
    Assert.Equal(1,result)

[<Fact>]
let ``????`` () =
    let sample = [
        "0"
        "1"
        "0"]    
    let result = calculateGamma sample
    Assert.Equal(0,result)

[<Fact>]
let ``?????`` () =
    
    let result = sample.Split(Environment.NewLine) |> Array.toList |>  calculateGamma 
    Assert.Equal(22,result)

[<Fact>]
let ``epsilon`` () =
    
    let result = 
        sample.Split(Environment.NewLine)
        |> Array.toList
        |>  calculateGamma
        |> getEpsilon 
    Assert.Equal(9,result)
