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

let fuzzyAND (a:float) b = a * b

let fuzzyNOT (a:float) = 1. - a

let fuzzifyVar (fs: (string * (((float * float) list) option)) list) (x: float) = 
    // membership function (y) of each value for one single input (x) 
    let rec mfValue (x:float) (points: (float * float) list) =
        match points with
        | p1 :: p2 :: _ when X(p1) <= x && x <= X(p2) -> 
            Y(p1) + (x - X(p1)) * ( Y(p2) - Y(p1) )/( X(p2) - X(p1) ) // linear interpolation
        | p :: [] -> Y(p) // singleton (one point fuzzy set) defuzzifier case or exceded max value in set
        | [] -> 0.0
        | _ :: rest -> mfValue x rest

    [
        for value in fs do
            match value with
            | valName, Some points -> 
                match points with
                | firstPoint :: _ when x < X(firstPoint) -> yield (valName, Y firstPoint) // the value is lower than expected in fuzzy set
                | _ -> yield (valName, mfValue x points)
            | valName, _ -> yield (valName, 0.0) 
    ]

