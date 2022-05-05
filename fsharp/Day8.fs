module Day8

open System.Text
open Xunit
open FsUnit.Xunit
open FParsec
open FSharpx.String


let wordsSeparatedBySpaces = sepBy (manyChars letter) (pstring " ")
let lineParser = pipe4 wordsSeparatedBySpaces (pstring "|" .>> spaces)  wordsSeparatedBySpaces newline  (fun input _ output _ -> (input,output)) 
let inputParser = many lineParser


let sample = """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
"""

let resultOrDie = function
            | Success(result, _, _) -> result
            | Failure(s, _, _) -> failwith s

let count1478 input =
    let oneIf1478and0 (s:string) =
        match s.Length with
        | 2|3|4|7 -> 1
        | _ -> 0
    input |> List.collect snd |> List.sumBy oneIf1478and0  

[<Fact>]
let ``Part 1 from sample``() =
    let input = run inputParser sample |> resultOrDie
    count1478 input |> should equal 26
    
[<Fact>]
let ``Day 8 part 1``() =
    let input = runParserOnFile inputParser () "day8Input.txt" System.Text.Encoding.UTF8 |> resultOrDie
    count1478 input |> should equal 493
    

let lineSample = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
"


let sumOfAllDisplays samples =
    let toWiringSets = List.map (toCharArray >> Set.ofArray)
    
    let transcode sets x =        
        let setsByLength = List.groupBy Set.count sets |> Map.ofList
        
        let one = setsByLength[2] |> List.exactlyOne
        let four = setsByLength[4] |> List.exactlyOne
        let seven = setsByLength[3] |> List.exactlyOne
        let eigth = setsByLength[7] |> List.exactlyOne
    
        let nine = setsByLength[6] |> List.filter (fun s -> Set.isSuperset s four) |> List.exactlyOne
        let zero = setsByLength[6] |> List.filter (fun s -> s <> nine && Set.isSuperset s one) |> List.exactlyOne
        let six = setsByLength[6] |> List.filter (fun s -> s <> nine && s <> zero) |> List.exactlyOne
        
        let three = setsByLength[5] |> List.filter (fun s -> Set.isSuperset s one) |> List.exactlyOne
        let five = setsByLength[5] |> List.filter (fun s -> Set.isSuperset nine s && s <> three) |> List.exactlyOne
        let two = setsByLength[5] |> List.filter (fun s -> s <> three && s <> five) |> List.exactlyOne
        
        let toDigitMap = Map [ (one, "1"); (two, "2"); (three, "3"); (four, "4"); (five, "5"); (six, "6"); (seven, "7"); (eigth, "8"); (nine, "9"); (zero, "0") ]
        toDigitMap[x]
    
    let toPairOfWiringSets (g,d) = toWiringSets g, toWiringSets d
    let toTranscoderAndInputs (g,d) = transcode g, d
    let toDigits (transcoderFunction,inputs) = List.map transcoderFunction inputs
    let toNumber = String.concat "" >> int
    
    samples |> List.sumBy (toPairOfWiringSets >> toTranscoderAndInputs >> toDigits >> toNumber)
    
[<Fact>]
let ``Sum of a line``() =
    let input = run inputParser lineSample |> resultOrDie
    sumOfAllDisplays input |> should equal 5353
    
[<Fact>]
let ``Sum for sample``() =
    let input = run inputParser sample |> resultOrDie
    sumOfAllDisplays input |> should equal 61229
    
[<Fact>]
let ``Day 8 part 2``() =
    let input = runParserOnFile inputParser () "day8Input.txt" Encoding.UTF8 |> resultOrDie
    sumOfAllDisplays input |> should equal 1010460
