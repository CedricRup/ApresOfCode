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

let fromCharToNumber = string >> int

let getColumnFromReport (diagnosticReport: String list) column   =
        diagnosticReport |> List.map (fun word -> word[column] |> fromCharToNumber)

let determineMostCommonBit list =
    let folder (numberOf1, count) bit =
            (numberOf1+bit, count+1)
    let numberOf1, count = List.fold folder (0,0) list 
    if 2 * numberOf1 >= count then 1 else 0  

let fromBitWordToNumber = List.reduce (fun word bit-> (word <<< 1) + bit) 

let calculate transform (diagnosticReport: String list)  =

    let wordLength = diagnosticReport.Head.Length
    let getColumnFromThisReport = getColumnFromReport diagnosticReport

    [0.. wordLength-1] 
    |> List.map (getColumnFromThisReport >> determineMostCommonBit >> transform)
    |> fromBitWordToNumber

let reverse bit = if bit = 0 then 1 else 0 

let calculateGamma = calculate id

let calculateEpsilon = calculate reverse
    

[<Fact>]
let ``calculateGamma on 1 bit word where 1 wins gives 1`` () =
    let sample = [
        "1"
        "0"
        "1"
    ]    
    let result = calculateGamma sample
    Assert.Equal(1,result)

[<Fact>]
let ``calculateGamma on 1 bit word where 0 wins gives 0`` () =
    let sample = [
        "0"
        "1"
        "0"]    
    let result = calculateGamma sample
    Assert.Equal(0,result)

[<Fact>]
let ``calculateGamma on sample`` () =
    let result = sample.Split(Environment.NewLine) |> Array.toList |>  calculateGamma 
    Assert.Equal(22,result)

[<Fact>]
let ``epsilon on sample`` () =
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



let calculateAdvancedRate transform (diagnosticReport: String list) =
    let rec filterByMostCommonBitInColumn (diagnosticReport: String list) column =
        let mostCommonBitInChar = 
            getColumnFromReport diagnosticReport column
            |> determineMostCommonBit
            |> transform
            |> string
            |> char
        let remainingWords = diagnosticReport |> List.filter (fun word -> word[column]  = mostCommonBitInChar)
        match remainingWords with
            | [] -> failwith "Bang"
            | [word] -> word
            | array -> if (array.Head.Length > (column + 1)) then filterByMostCommonBitInColumn remainingWords (column+1) else array[0] 

    let word = filterByMostCommonBitInColumn diagnosticReport 0
    word.ToCharArray() |> Array.map (string >> int) |> Array.toList |> fromBitWordToNumber

let calculateOxygenGeneratorRate = calculateAdvancedRate id
let calculateCO2ScrubberRate = calculateAdvancedRate reverse

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
let ``oxygen generator rating on 2 bits words`` () =
    let sample = [
        "01"
        "11"
        "00"]    
    let result = calculateOxygenGeneratorRate sample
    Assert.Equal(1,result)
    
[<Fact>]
let ``oxygen generator rating from sample``() =
    let result = 
        sample.Split(Environment.NewLine)
        |> Array.toList
        |> calculateOxygenGeneratorRate 
    Assert.Equal(23,result)
    
[<Fact>]
let ``CO2 scrubber rating from sample``() =
    let result = 
        sample.Split(Environment.NewLine)
        |> Array.toList
        |> calculateCO2ScrubberRate 
    Assert.Equal(10,result)
    
[<Fact>]
let ``Day 3 part 2`` () =
    let report =
        IO.File.ReadAllLines "day3Input.txt"
        |> Array.toList
    let oxygen = calculateOxygenGeneratorRate report
    let co2 = calculateCO2ScrubberRate report 
    Assert.Equal(4672151,oxygen*co2)
