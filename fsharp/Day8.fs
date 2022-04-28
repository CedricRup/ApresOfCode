module Day8

open Xunit
open FsUnit.Xunit
open FParsec


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
        match (s.Length) with
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
    

let lineSample = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"


let sumOfAllDisplays samples =
    let line = List.head samples |> fst
    let sets = line  |> List.map Set.ofList
    let setsByLength = List.groupBy Set.count sets |> Map.ofList
    
    let one = setsByLength[2] |> List.exactlyOne
    let four = setsByLength[4] |> List.exactlyOne
    let seven = setsByLength[3] |> List.exactlyOne
    let eigth = setsByLength[7] |> List.exactlyOne
   
    let nine = setsByLength[6] |> List.filter (fun s -> Set.contains s four) |> List.exactlyOne
    let zero = setsByLength[6] |> List.filter (fun s -> s <> nine && Set.contains s one) |> List.exactlyOne
    let six = setsByLength[6] |> List.filter (fun s -> s <> nine && s <> zero) |> List.exactlyOne
    0
      
//2 3 5 ont meme nombre
//3 inclus 1
//9 inclus 5
//2 est celui qui reste*)  
    
    
[<Fact>]
let ``Sum of a line``() =
    let input = run inputParser lineSample |> resultOrDie
    sumOfAllDisplays input |> should equal 5353
    
    
(*0 1 3 3 4 5 6 7 8 9 

6 et 9 0 ont 6 segments,
9 à les segment de 4
Le 0 contient le 1
2 3 5 ont meme nombre
3 inclus 1
9 inclus 5
2 est celui qui reste*)