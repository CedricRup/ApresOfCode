module fsharp.Common

open FParsec.Primitives
open FParsec.CharParsers

type UserState = unit // doesn't have to be unit, of course
type Parser<'t> = Parser<'t, UserState>
let intSeparatedByColumnParser : Parser<int32 list> = sepBy pint32 (pstring ",")

let resultOrDie = function
            | Success(result, _, _) -> result
            | Failure(s, _, _) -> failwith s

