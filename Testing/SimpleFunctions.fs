module SimpleFunctions
// NOT PARSERS
open System
open System.IO
open TypeDefinitions
open System.Security.Claims


// alike fst and snd
let trd a b c = c

let X = fst
let Y = snd

// curried version of str.Trim()
let trim (str:string) = str.Trim()

let noneIfEmpty = function
    | [] -> None
    | vl -> Some vl

// Interpreter
// ugly solution to lack of declaration - making evaluation function  
let declare<'a>  =  Unchecked.defaultof<'a>

let rec recursiveFix f x = f (recursiveFix f) x

let toNearest (n:float) = Math.Round n

let rec factorial = 
    function
    | n when toNearest n = 0. -> 1.
    | n when toNearest n >= 1. -> n * factorial(n - 1.)
    | n when toNearest n < 0. -> nan

