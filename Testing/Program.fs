// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
// to open in F# Interactive
// #r """C:\Users\Tomasz\source\repos\fparsec-master\FParsec\bin\Debug\net45\FParsecCS.dll"""
// #r """C:\Users\Tomasz\source\repos\fparsec-master\FParsec\bin\Debug\net45\FParsec.dll"""

module TestingES

open FParsec
open System
open System.IO
open System.Text
open FSharpx.Collections
open Microsoft.FSharp.Collections
open System.Collections.Generic

////////// IMPORT OF INPUT TEXT

let errorMsg = new StringBuilder()
errorMsg.Append "\r\n --- Warnings and errors section ---\r\n\r\n" |> ignore
let commandLog = new StringBuilder()
commandLog.Append "\r\n --- Used commands section ---\r\n\r\n" |> ignore

// Which file to load?
let demoProgramFile = "DEMO.txt"
printfn "Demo file is in \"Testing\\Testing\\\" path."
printfn "Write the file name (with .txt) for parsing or type ENTER for %s " demoProgramFile
printfn "UTF-8 files allowable only"

let inputString = 
    try
        let fileName = System.Console.ReadLine()
        let filePath:string = __SOURCE_DIRECTORY__ + "\\" + fileName
        removeComments (File.ReadAllText(filePath, Encoding.UTF8))
    with
        :? System.IO.FileNotFoundException | :? System.IO.DirectoryNotFoundException -> 
            let filePath2:string = __SOURCE_DIRECTORY__ + (sprintf "\\%s" demoProgramFile)
            printfn "Chosen default file - %s" demoProgramFile
            removeComments (File.ReadAllText(filePath2, Encoding.UTF8))


//File.WriteAllText (__SOURCE_DIRECTORY__ + "\\no_comments.txt", (inputString)) //debug only


// INITIALIZING global functions and variables, resizable arrays for qualifiers, choices and variables

let mutable basicInfo = 
    {
        Subject = None |> Subject
        Author = None |> Author
        StartingText = None |> StartingText
        EndingText = None |> EndingText
        ExternalProgram = None |> ExternalProgram
        DisplayThreshold = 0.0 |> Threshold
        Probability = ProbabilityMode.Fuzzy |> Probability
        InitChoiceConfidence = 0.0 |> Confidence
        DisplayRules = false |> DisplayRules
        Derivation = Derivation.AllRulesUsed |> BasicAttribute.Derivation
        FuzzyThreshold = 0.1 |> FuzzyThreshold
        LoopOverVariable = None |> LoopOver
        CSVDelimiter = "," |> Seq.toArray |> CSV 
        OrCurrentValue = false |> OrCurrent
    }
let mutable qualifiers = new ResizeArray<Qualifier>()
let mutable choices = new ResizeArray<Choice>()
let variables = new ResizeArray<Variable>()
let mutable rules = new ResizeArray<Rule>()

// Resize array auxiliary functions and parsers
let manyRA p (arr: ResizeArray<_>) =
    // the compiler expands the call to Inline.Many to an optimized sequence parser. Enable to use "many" with resize array rather than list.
    Inline.Many(
        elementParser = p,
            stateFromFirstElement = (fun x0 ->
                                        arr.Add(x0)
                                        arr),
            foldState = (fun ra x -> ra.Add(x); ra),
            resultFromState = (fun ra -> ra),
            resultForEmptySequence = (fun () -> arr)
        )

// Set of functions indexing records of specific type in resize array
let indexElementsOfRules (arr: ResizeArray<_>) =
    ResizeArray.mapi (fun i (record: Rule) -> { record with Number = i + 1 }) arr 
let indexElementsOfQualifiers (arr: ResizeArray<_>) =
    ResizeArray.mapi (fun i (record: Qualifier) -> { record with Number = i + 1 }) arr
let indexElementsOfChoices (arr: ResizeArray<_>) =
    ResizeArray.mapi (fun i (record: Choice) -> { record with Number = i + 1 }) arr

// RA version of isString()
let isStringRA (str:string) = 
    ResizeArray.exists ( fun (v:Variable) -> 
        let (VarAttribute.Name nameofVar) = v.Name
        match v.Value with
            | StringVariable s -> str = nameofVar
            | _ -> false) variables
// returns None if variable doesn't exists, option with this structure if it does
let isVariable x = 
    ResizeArray.tryFind (fun (a:Variable) -> 
        match a.Name with 
        | Name n -> n = x
        | _ -> false ) variables
       
// Set given rule in "rules" RA with given boolean value 
let setRule (rule:Rule) (isTrue:float) =
    let (FuzzyThreshold fuzzyThreshold) = basicInfo.FuzzyThreshold
    let index = ResizeArray.tryFindIndex (fun i -> rule.Number = i.Number) rules
    match index with
    | Some i when isTrue >= fuzzyThreshold -> rules.Item i <- {rule with State = (True fuzzyThreshold)}
    | Some i -> rules.Item i <- {rule with State = False}
    | _ -> failwith (sprintf "No such rule defined %A" rule)
    
