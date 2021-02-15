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

let defuzzify (qualifier:Qualifier) (inputValues: (string * float)list) =
    // if defuzzification has to be height method
    let rec findCentersOfSets (acc: (string * float) list) (enums: (string * (((float * float) list) option))list) =
        match enums with
        | ( valueName, Some(points) ) :: restEnums ->
            let centralX = 
                [ for point in points do // find supremum points
                    if points <> [] && Y(point) = Y(List.maxBy Y points)
                    then yield point ]
                |> function
                | [] -> failwith (sprintf "No max y value in %A?" qualifier.unwrapQuestion)
                | max :: [] -> X(max) // one maximum point
                | maxPoints -> // take average (center) of xMax1 and xMax2
                    [ List.minBy X maxPoints; List.maxBy X maxPoints ]
                    |> List.averageBy X
            findCentersOfSets ((valueName, centralX) :: acc) restEnums    
        | [] -> acc
  
    let centersOfSets = findCentersOfSets [] qualifier.Enumerations

    let rec sum numerator denominator (centersX: (string * float) list) (valuesY: (string * float) list) =
        match valuesY with
        | (name, valueY) :: restY -> 
            let congruentX = snd(List.find (function centerX -> fst(centerX) = name) centersX)
            if valueY >= 0. then sum (congruentX * valueY + numerator) (valueY + denominator) centersX restY
            else  sum (numerator) (denominator) centersX restY
        | [] -> 
            if denominator = 0. then 0.
            else numerator/denominator

    sum 0. 0. centersOfSets inputValues


let csvToNumber path (row:float) (column:float) (delimiter) =
    let csvInLines = System.IO.File.ReadAllLines(path)
    try csvInLines.[(int(row))].Split(delimiter).[(int(column) - 1)]
    with :? System.IndexOutOfRangeException -> "0"
    |> float


let saveToCsv path (row:float) (column:float) (delimiter) (input:float) =
    let currentCsv = System.IO.File.ReadAllLines(path)
    let maxColumn = 
        try Array.max [| for currentRow in currentCsv do yield currentRow.Split(delimiter).Length |]
        with :? ArgumentException -> int column
    let newCsv = 
        if  row > float(currentCsv.GetLength(0) - 1) || column > float(maxColumn) then
            Array.unfold (fun i ->
                if i <= (max (int row) (currentCsv.Length - 1)) then 
                    (
                        Array.unfold ( fun j -> 
                            if j <= (max (int column - 1) (maxColumn - 1)) then 
                                try 
                                    (currentCsv.[i].Split(delimiter).[j] , j+1) |> Some
                                with
                                    :? System.IndexOutOfRangeException -> ("0", j+1) |> Some
                            else None
                        ) 0
                    , 
                    (i+1)
                    ) |> Some
                else None
                ) 1
            (* NOT WORKING BECAUSE OF EXCEPITION HANDLING IN SEQUENCE EXPRESSIONS[|
                for i = 1 to (max (int row) (currentCsv.Length - 1)) do
                    yield 
                        [|  
                            for j = 0 to (max (int column) maxColumn) do 
                                try 
                                    yield currentCsv.[i].Split(delimiter).[j]
                                with 
                                    :? System.IndexOutOfRangeException -> yield "0"
                        |] 
            |]*)
        else 
            [| 
                for currentRow in currentCsv.[1..(currentCsv.Length - 1)] do 
                    yield [| for cell in currentRow.Split(delimiter) do yield cell |] 
            |]

    (newCsv.[int(row - 1.)].[int(column - 1.)] <- input.ToString())
    let delimiterString = String.Concat delimiter
    let csvString =
        let firstRow = 
            try currentCsv.[0]
            with :? System.IndexOutOfRangeException -> 
                String.Join("",[| for i = 0 to newCsv.[0].Length do yield (sprintf "Col-%i" i) |])
        [| 
            yield firstRow
            for newRow in newCsv do 
                yield ( String.Join(delimiterString, newRow ))
        |]    
    System.IO.File.WriteAllLines(path,csvString) 

