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
    
// Initializing dictionaries for general variables
// option case Some ... means that the value evaluation is done and assured. The value is accessible directly from dictionary 
// option case 'None' means not initialized var which should be evaluated by four level "Variable Evaluation Mechanism" but is declared
let qualifierDict = new Dictionary<string,(string * float) list option>() // List of possible states, by body (question) string
let numericVariableDict = new Dictionary<string,float option>()
let stringVariableDict = new Dictionary<string,string option>()
let choiceDict = new Dictionary<string, float option>()

// Qualifiers and variables query functons
let findQualifierByQuestion q : Qualifier option = 
    ResizeArray.tryFind (fun qualifier -> qualifier.unwrapQuestion = q) qualifiers 

// Test for parsed keys and values
let isString str = stringVariableDict.ContainsKey str
let isNumeric str = numericVariableDict.ContainsKey str
let isQualifier str = qualifierDict.ContainsKey str
let isQualifierName str = ResizeArray.exists (function (q:Qualifier) -> q.unwrapName = str) qualifiers
let isEnum (qualifierQuestion:string) strList = 
    match ResizeArray.tryFind (fun (q:Qualifier) -> q.unwrapQuestion = qualifierQuestion) qualifiers with
    | Some qualifier -> 
        if List.forall (fun s -> List.contains s qualifier.unwrapEnums) strList then true else false
    | None -> false
let isEnumDefined key str = 
    if isQualifier key then
        match qualifierDict.Item key with
        | Some items -> List.exists (fun item -> fst item = fst str) items
        | None -> false
    else false
let inRange key f =
    match ResizeArray.tryFind (fun (v:Variable) -> v.unwrapName = key && isNumeric key) variables with     
    | Some numVar -> fst numVar.getRange <= f && f <= snd numVar.getRange
    | None -> failwith (sprintf "Variable %s doesn't exists or is not a numeric variable" key)
let isChoice str = ResizeArray.exists (fun (c:Choice) -> c.Name = str) choices 
let confidenceInRange value = 
    match basicInfo.Probability with
    | Probability(ProbabilityMode.YesNo) when value = 1. -> Confidence.YesNo true
    | Probability(ProbabilityMode.YesNo) when value = 0.0 -> Confidence.YesNo false
    | Probability(ProbabilityMode.ZeroTen) when 0. <= value || value <= 10. -> Confidence.ZeroTen (int value)
    | Probability(ProbabilityMode.HundredAverage) when -100. <= value && value <= 100. -> Confidence.HundredScale (int value)
    | Probability(ProbabilityMode.IncrDecr) when -10_000. <= value && value <= 10_000. -> Confidence.IncrDecr (int value)
    | Probability(ProbabilityMode.Custom) -> Confidence.Custom (value)
    | Probability(ProbabilityMode.Fuzzy) when -1. <= value && value <= 1. -> Confidence.Fuzzy (value)
    | _ -> failwith "Improper value for choice"


////////// PARSERS DEFINITIONS
    
// BASIC INFO PARSER

let pBasic = 
    let pBasicSubject = key "Subject" ":" >>. opt ( pSentence) |>> Subject .>> ws
    let pBasicAuthor = key "Author" ":" >>. opt ( pSentence ) |>> Author .>> ws
    let pBasicStartText = strCI_ws "Starting " >>. key "text" ":" >>. opt ( pSentence ) |>> StartingText .>> ws
    let pBasicEndText = strCI_ws "Ending" >>. key "text" ":" >>. opt ( pSentence ) |>> EndingText .>> ws
    // place for external program parser
    let pBasicThreshold = strCI_ws "display" >>. key "threshold" ":" >>. pfloat |>> Threshold .>> ws
    // auxiliary for probability system parser
    let pModes = choice [attempt( attempt(str_ws "1") <|> (strCI_ws "yes" >>. strCI_ws "no") ) >>% ProbabilityMode.YesNo
                         attempt ( choice [ attempt(str_ws "2")
                                            ( attempt(strCI_ws "0") <|> strCI_ws "zero" >>. optional( attempt(strCI_ws "-") <|> strCI_ws "to" ) >>. optional( attempt(strCI_ws "ten") <|> strCI_ws "10"))
                                           ] >>% ProbabilityMode.ZeroTen)
                         attempt (  attempt(str_ws "3") <|> (strCI_ws "-100" >>? optional(strCI_ws "to") >>? optional(strCI_ws "100")) >>. 
                                    (attempt(strCI "D" >>. nextCharSatisfies (isAnyOf " \n\r") >>% ProbabilityMode.HundredDependent) .>> ws <??> "Expected newline after 'A', 'D' or 'I' letter" <|> 
                                     attempt(strCI "I" >>. nextCharSatisfies (isAnyOf " \n\r") >>% ProbabilityMode.HundredIndependent) .>> ws <??> "Expected newline after 'A', 'D' or 'I' letter" <|> 
                                     (optional(strCI "A" >>. nextCharSatisfies (isAnyOf " \n\r")) >>% ProbabilityMode.HundredAverage) .>> ws <??> "Expected newline after 'A', 'D' or 'I' letter"
                                     ) )
                         attempt (((strCI "Incr" >>? optional (strCI_ws "ement")) >>? strCI_ws "/" >>? (strCI "Decr" >>? optional (strCI_ws "ement"))) <|> (str_ws "4") >>% ProbabilityMode.IncrDecr)
                         attempt ((strCI_ws "custom" >>? optional(strCI_ws "formula")) <|> (str_ws "5") >>% ProbabilityMode.Custom)
                         ((strCI_ws "fuzzy" >>? optional (strCI_ws "logic")) <|> strCI_ws "6") >>% ProbabilityMode.Fuzzy
                         ]
    let pBasicProbability = strCI_ws "Probability" >>. optional(strCI_ws "system") >>. strCI_ws ":" >>. (pModes |>> Probability) .>> ws
    
    let pConfidence = strCI_ws "Initial" >>. strCI_ws "choice" >>. key "confidence" ":" >>. (pfloat |>> Confidence)
    let pDisplayRules = strCI_ws "display" >>. key "rules" ":" >>. ( attempt(strCI "y" >>. optional(strCI "es") >>% true) <|> (strCI "n" >>. optional(strCI "o") >>% false) ) |>> DisplayRules .>> ws
    let pBasicDerivation = key "Derivation" ":" >>. 
        choice [ attempt( (strCI_ws "a") >>. optional(strCI_ws "ll") >>. optional(strCI_ws "rules") >>. optional(strCI_ws "used") ) >>% Derivation.AllRulesUsed
                 attempt( (strCI_ws "n") >>. optional(strCI_ws "on") >>. optional(strCI_ws "-") >>. optional(strCI_ws "redundant") ) >>% Derivation.NonRedundant 
                 attempt( (strCI_ws "c") >>. optional(strCI_ws "hosen") >>. optional(strCI_ws "by") >>. optional(strCI_ws "user") ) >>% Derivation.ChosenByUser]
        |>> BasicAttribute.Derivation .>> ws
    let pFuzzy = strCI_ws "fuzzy" >>? key "threshold" ":" >>. pfloat |>> FuzzyThreshold .>> ws
