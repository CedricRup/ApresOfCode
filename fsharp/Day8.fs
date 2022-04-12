module Day8

open FSharpx.Collections
open Xunit
open FsUnit.Xunit
open FParsec
open fsharp.Common


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

let countInstanceOfSimpleDigits display =
    let isSimpleDigit (d:string) =
           match d.Length with
           | 2 | 4 | 3 | 7 -> true
           | _ -> false
    display |> List.collect snd |> List.filter isSimpleDigit |> List.length


let wordsSeparatedBySpaces = sepBy (manyChars letter) (pstring " ")
let lineParser =pipe4 wordsSeparatedBySpaces (pstring "|" .>> spaces)  wordsSeparatedBySpaces newline  (fun input _ output _ -> (input,output)) 
let inputParser = many lineParser

let decode (inputs,outputs) =
    let toSimpleDigits (string: string) =
        match string.Length with
        | 2 -> Some (1, set string)
        | 4 -> Some (4, set string)
        | 3 -> Some (7, set string)
        | 7 -> Some (8, set string)
        | _ -> None
    
    let digitToString = inputs |> List.choose toSimpleDigits |> dict
    
    let findUniqueHavingLengthAndContaining length mustContain =
        inputs |> List.filter (fun s -> s.Length = length && Set.isSubset mustContain (set s)) |> List.exactlyOne |> set
        
    
    let the3 = findUniqueHavingLengthAndContaining 5 digitToString[1]
    let a = digitToString[7] - digitToString[1]
    let b = digitToString[4] - the3
    let d = digitToString[4] - digitToString[1] - b
    let e = digitToString[8] - the3 - b
    let g = the3 - digitToString[7] - d
    let the5 = findUniqueHavingLengthAndContaining 5 b
    let c = digitToString[8] - the5 - e
    let f = digitToString[1] - c
    
    
    let transcoder =
        [
            ( a+b+c+e+f+g,0)
            ( c+f,1)
            ( a+c+d+e+g,2)
            ( a+c+d+f+g,3)
            ( b+c+d+f,4)
            ( a+b+d+f+g,5)
            ( a+b+d+e+f+g,6)
            ( a+c+f,7)
            ( a+b+c+d+e+f+g,8)
            ( a+b+c+d+f+g,9)
        ] |> dict
    
    let transcode s =
        Dictionary.tryFind (set s) transcoder |> Option.get |> string |> int
    let fromDigitsToNumber = List.reduce (fun result i -> (result * 10) + i)
    outputs |> List.map transcode |> fromDigitsToNumber  

let sumOfDecode inputs =
    inputs |> List.sumBy decode


[<Fact>]
let ``Count simple digit from sample``() =
    run inputParser sample |> resultOrDie |> countInstanceOfSimpleDigits |> should equal  26
    
[<Fact>]
let ``Day 8 part 1``() =
    runParserOnFile inputParser () "day8Input.txt" System.Text.Encoding.UTF8
    |> resultOrDie
    |> countInstanceOfSimpleDigits |> should equal  493

[<Fact>]
let ``decode one sample`` () =
    let sample = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf\r\n"
    let input = run lineParser sample |> resultOrDie
    let result = decode input
    result |> should equal 5353
    
[<Fact>]
let ``decode full sample`` () =
    let input = run inputParser sample |> resultOrDie
    let result = sumOfDecode input
    result |> should equal 61229
    
[<Fact>]
let ``Day 8 part 2`` () =
    runParserOnFile inputParser () "day8Input.txt" System.Text.Encoding.UTF8
    |> resultOrDie
    |> sumOfDecode
    |> should equal 1010460