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

// smallest part for floating point numbers comparisons, especially equality
let epsilon = 0.000_001 //Double.Epsilon * 50_000.
// equality with tolerance
let (=~) a b = abs(a - b) < epsilon 

// important for qualifier comparisons
let containsAnyOf options vl = // TRUE if "options" contains any of "vl" 
    List.exists (fun v -> List.contains v options) vl

let sortOutPoints (points: (float * float) list) = 
    List.sortBy (fun point -> fst point) points 

let fuzzyOR (a:float) b = 
    if (a > 0. || a =~ 0.) && (b > 0. || b =~ 0.) then 
        a + b - a * b
    elif (a < 0. && b < 0.) then 
        - ( (-a) + (-b) - (-a)*(-b) )
    else
        (a + b) / (1. - (min (abs a) (abs b)))

