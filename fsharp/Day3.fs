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

let getColumnFromReport (diagnosticReport: String list) column   =
        diagnosticReport |> List.map (fun word -> word[column])

let determineMostCommonBit (values:int*int) numberOfSamples list =
    let sum = list |> List.sumBy (string>>int)
    if 2 * sum >= numberOfSamples then fst values else snd values  


let calculate (values:int*int) (diagnosticReport: String list)  =

    let numberOfSamples = List.length diagnosticReport

    
    let wordLength = diagnosticReport.Head.Length

    [0.. wordLength-1] 
    |> List.map (getColumnFromReport diagnosticReport >> (determineMostCommonBit values numberOfSamples))
    |> List.reduce (fun word bit-> (word <<< 1) + bit)


let calculateGamma = calculate (1, 0)

let calculateEpsilon = calculate (0, 1)
    

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
        |> calculateEpsilon 
    Assert.Equal(9,result)


[<Fact>]
let ``Day 3 part 1`` () =
    
    let report =
        IO.File.ReadAllLines "day3Input.txt"
        |> Array.toList
    let gamma = calculateGamma report
    let epsilon = calculateEpsilon report 
    Assert.Equal(1092896,gamma*epsilon)



let calculateOxygenGeneratorRate (diagnosticReport: String list) =
    let rec glop (diagnosticReport: String list) column =
        let numberOfSamples = List.length diagnosticReport
        let mostCommonBit = 
            getColumnFromReport diagnosticReport column
            |> determineMostCommonBit (1,0) numberOfSamples
        let remainingWords = diagnosticReport |> List.filter (fun word -> ( string word[column] |> int) = mostCommonBit)
        match remainingWords with
            | [] -> failwith "Bang"
            | [word] -> word
            | array -> if (array.Head.Length > (column + 1)) then glop remainingWords (column+1) else array[0] 

    let word = glop diagnosticReport 0
    word.Split() |> Array.toList |> List.map int  |> List.reduce (fun word bit-> (word <<< 1) + bit)



[<Fact>]
let ``oxygen generator rating on one bit word with equality 1 wins`` () =
    let sample = [
        "0"
        "1"]    
    let result = calculateOxygenGeneratorRate sample
    Assert.Equal(1,result)

[<Fact>]
let ``oxygen generator rating on one bit word`` () =
    let sample = [
        "0"
        "1"
        "1"]    
    let result = calculateOxygenGeneratorRate sample
    Assert.Equal(1,result)

[<Fact>]
let ``oxygen generator rating on one bit word where 0 wins`` () =
    let sample = [
        "0"
        "1"
        "0"]    
    let result = calculateOxygenGeneratorRate sample
    Assert.Equal(0,result)

[<Fact>]
let ``oxygen generator rating ????`` () =
    let sample = [
        "01"
        "11"
        "00"]    
    let result = calculateOxygenGeneratorRate sample
    Assert.Equal(1,result)
