//Remember to use following command in packet manager before building up the project
//Update-Package –reinstall FParsec

module TestingES

open FParsec
open System
open System.IO
open System.Text
open FSharpx.Collections
open Microsoft.FSharp.Collections
open System.Collections.Generic

// custom modules
open TypeDefinitions
open SimpleFunctions
open SimpleParsers
open Printers


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
    let pLoopOver = 
        strCI_ws "Loop" >>. optional(strCI_ws "over") >>. optional(strCI_ws "variable") >>. str_ws ":" 
        >>. ( pAnyString ) |>> Some |>> LoopOver .>> ws
    let pCSVDelimiter = strCI_ws "CSV" >>. optional(strCI_ws "delimiter") >>. str_ws ":" >>. (pSentence |>> trim |>> Seq.toArray |>> CSV) .>> ws
    let pOrCurrent = 
        strCI_ws "or" >>. optional(strCI_ws "current") >>. optional(strCI_ws "value") >>. str_ws ":" >>. 
        ( attempt(strCI "y" >>. optional(strCI "es") >>% true |>> OrCurrent) <|> (strCI "n" >>. optional(strCI "o") >>% false |>> OrCurrent) ) .>> ws

    let pAttributes = many( choice [attempt pBasicSubject
                                    attempt pBasicAuthor
                                    attempt pBasicStartText
                                    attempt pBasicEndText
                                    attempt pBasicThreshold
                                    attempt pBasicProbability
                                    attempt pConfidence
                                    attempt pDisplayRules
                                    attempt pBasicDerivation
                                    attempt pLoopOver
                                    attempt pCSVDelimiter
                                    attempt pOrCurrent
                                    pFuzzy])
    pAttributes |>> ( fun basicInfo -> 
        {
            Subject = 
                match List.tryFind (function Subject _ -> true | _ -> false) basicInfo with
                    | Some sub -> sub
                    | _ -> None |> Subject
            Author = 
                match List.tryFind (function Author _ -> true | _ -> false) basicInfo with
                        | Some author -> author
                        | _ -> None |> Author
            StartingText = 
                match List.tryFind (function StartingText _ -> true | _ -> false) basicInfo with
                        | Some st -> st
                        | _ -> None |> StartingText
            EndingText = 
                match List.tryFind (function EndingText _ -> true | _ -> false) basicInfo with
                        | Some et -> et
                        | _ -> None |> EndingText
            ExternalProgram = None |> ExternalProgram
            InitChoiceConfidence = 
                match List.tryFind (function Confidence _ -> true | _ -> false) basicInfo with
                    | Some mode -> mode 
                    | None -> 0.0 |> Confidence
            Derivation =
                match List.tryFind (function Derivation _ -> true | _ -> false) basicInfo with
                    | Some deriv -> deriv
                    | _ -> Derivation.AllRulesUsed |> BasicAttribute.Derivation
            Probability = 
                match List.tryFind (function Probability _ -> true | _ -> false) basicInfo with
                    | Some prob -> prob
                    | _ -> ProbabilityMode.YesNo |> Probability
            DisplayThreshold = 
                match List.tryFind (function Threshold _ -> true | _ -> false) basicInfo with
                    | Some thres -> thres
                    | _ -> 0.0 |> Threshold
            FuzzyThreshold =
                match List.tryFind (function FuzzyThreshold _ -> true | _ -> false ) basicInfo with
                    | Some(FuzzyThreshold fuzzy)  when -1. < fuzzy && fuzzy <= 1. -> fuzzy |> FuzzyThreshold
                    | _ -> 
                        errorMsg.AppendLine "Incorrect fuzzy threshold. Selected default = 0.1" |> ignore
                        0.1 |> FuzzyThreshold
            DisplayRules = 
                match List.tryFind (function DisplayRules _ -> true | _ -> false) basicInfo with
                    | Some dr -> dr
                    | _ -> false |> DisplayRules
            LoopOverVariable = 
                match List.tryFind (function LoopOver _ -> true | _ -> false) basicInfo with
                    | Some var -> var
                    | _ -> None |> LoopOver
            CSVDelimiter = 
                match List.tryFind (function CSV _ -> true | _ -> false) basicInfo with
                | Some csv -> csv
                | None -> "," |> Seq.toArray |> CSV
            OrCurrentValue = 
                match List.tryFind (function OrCurrent _ -> true | _ -> false) basicInfo with
                | Some orCurrent -> orCurrent
                | None -> false |> OrCurrent
            
        })  

let CSVget path row column = 
    let (CSV delimiter) = basicInfo.CSVDelimiter
    csvToNumber path row column (delimiter)

let CSVset path row column input = 
    let (CSV delimiter) = basicInfo.CSVDelimiter
    saveToCsv path row column delimiter input

// END OF BASIC INFO PARSER


// QUALIFIERS PARSER

let pQualifier = 
    //parses one occurence of qualifier
    let pQualifierQuestion = 
        str_ws "Q>" >>. ( ((strCI_ws "run" >>? parentheses pSentence2 .>>? ws .>>.? (pSentence |>> trim)) |>> FromProgram) <|> (pSentence |>> FromRule) ) .>> ws
    let pMembershipFunction = 
        sepBy1 (parentheses( pRealNumber .>> str_ws "," .>>. (pIfTested (fun num -> 0. <= num && num <= 1.) "Membership function must be composed of numbers between 0 and 1" pRealNumber))) (str_ws ",") 
        |>> List.sortBy (fun point -> fst point)
        |> betweenCurly 
        |> opt // Fuzzy set points parser -  >V Some {(-inf,0), (0,0), (2,1), (4,1), (6,0), (inf, 0)}
        |> pIfTested (fun _ -> basicInfo.Probability = (ProbabilityMode.Fuzzy |> Probability)) "Parsing of fuzzy sets is available only in fuzzy logic mode (Probability mode: 6 in basic informations on the top)" 
    let pQualifierEnumerations = many (str_ws "V>" >>. (pSentence |>> trim .>> ws .>>. pMembershipFunction) .>> ws)
    
    let pQualifierName = key "name" ":" >>. (pAnyString >>= fun str -> preturn (str, "NAME") .>> ws ) 
    let pFuzzify = key "fuzzify" ":" >>. (pAnyString >>= fun str -> preturn (str, "FUZZIFY") .>> ws) 
    let pDefuzzify = key "defuzzify" ":" >>. (pAnyString >>= fun str -> preturn (str, "DEFUZZIFY") .>> ws) 
    let pAttributes = many(choice [attempt pQualifierName; attempt pFuzzify; attempt pDefuzzify]) .>> ws

    ( fun a b (c: (string * string) list) -> 
        let mutable fuzzifyVar = ""
        match a with
        | FromRule question ->
            if isQualifier question then 
                failwith (sprintf "Such qualifier already exists: %A.\n" question) 
            else 
                qualifierDict.Add(question, None)
        | FromProgram(question = q) -> qualifierDict.Add(q, None)
        { Number = 0 
          Question = a 
          Value = QualifierValue.Undefined
          Enumerations = b 
          Name =  match List.tryFind (function _ , "NAME" -> true | _ -> false) c with
                  | Some(str, _) -> Some str
                  | _ -> None
          Fuzzify = match List.tryFind (function _ , "FUZZIFY" -> true | _ -> false) c with
                    | Some(str, _) ->
                        fuzzifyVar <- str
                        Some str
                    | Some(str, _) -> Some str
                    | _ -> None
          Defuzzify = match List.tryFind ( function _ , "DEFUZZIFY" -> true | _ -> false) c with
                      | Some(str, _) when str <> fuzzifyVar -> Some str
                      | Some(str, _) -> failwith (sprintf "Fuzzify variable cannot be the same as defuzzify variable")
                      | _ -> None
          } )
    |> pipe3 pQualifierQuestion pQualifierEnumerations pAttributes
 

let manyQualifiers = 
    //parses many qualifiers occuring after keyword
    key "qualifiers" ":" >>. (manyRA pQualifier qualifiers) |>> (fun qRA -> qualifiers <- indexElementsOfQualifiers qualifiers; qualifiers) .>> ws
 
// END OF QUALIFIERS PARSER 


// CHOICES PARSER

let pChoice =
    str_ws "C>" >>. pAnyString |>> (fun a -> choiceDict.Add(a, None); { Number = 0; Name = a; State = None })


let manyChoices = 
    // parses many choices
    key "choices" ":" >>. (manyRA pChoice choices) |>> (fun chRA -> indexElementsOfChoices choices; choices ) .>> ws

// END OF CHOICES PARSER


// VARIABLES PARSER

let pVariable = 
    let pName = betweenSquare pAnyString
    let pDescription = pSentence
    let pType = key "Type" "=" >>. (attempt (pchar 'N') <|> pchar 'S') .>> ws
    // all three parsers for creating attributes with identifier that may be applied in any order
    let pInitNum = key "Initialize" "=" >>? (opt pfloat |>> NumericVariable) .>> ws 
    let pInitStr = key "Initialize" "=" >>? (betweenQuotations(opt pSentence |>> StringVariable)) .>> ws
    let pUpper = strCI_ws "Upper" >>. optional(strCI_ws "limit") >>. strCI_ws "=" >>. (opt pfloat) |>> Upper .>> ws |> notEmpty
    let pLower = strCI_ws "Lower" >>. optional(strCI_ws "limit") >>. strCI_ws "=">>. (opt pfloat) |>> Lower .>> ws |> notEmpty
    
    let pRest = many(choice [attempt(pInitNum); attempt(pInitStr); pUpper; pLower]) 

    (fun a b c (d:VarAttribute list) -> 
            { 
                Name = 
                    if isQualifier a || isString a || isNumeric a then 
                        failwith "Such string variable already exists!" 
                    else 
                        a |> Name;
                Description = b |> Description
                Value = match c with 
                        | 'S' -> 
                            let str = List.tryFind ( function | StringVariable _ -> true | _ -> false ) d // scan list of rest parameters
                            match str with // finded option with initialized value
                            | Some x -> 
                                let (StringVariable dictInput) = x // only one case possible
                                stringVariableDict.Add(a, dictInput) // bare option to the dict - Some if initialized
                                x // for record
                            | _ -> 
                                stringVariableDict.Add(a, None)
                                StringVariable None// ArgumentNullException - handled by FParsec
                        | 'N' -> 
                            let num = List.tryFind ( function | NumericVariable _ -> true | _ -> false ) d 
                            match num with // option of option
                            | Some x -> 
                                let (NumericVariable dictInput) = x 
                                numericVariableDict.Add(a, dictInput)
                                x 
                            | _ -> 
                                numericVariableDict.Add(a, None)
                                NumericVariable None 
                        | _ -> 
                            errorMsg.AppendFormat ("\tWrong indicator. Type should be denoted by adding 'Type = S' or 'Type = N' line\r\n") |> ignore
                            Undefined
                UpperLimit = 
                    try 
                        List.find ( function | Upper x -> true | _ -> false ) d 
                    with
                        :? System.Collections.Generic.KeyNotFoundException -> None |> Upper
                LowerLimit = 
                    try 
                        List.find ( function | Lower x -> true | _ -> false ) d 
                    with
                        :? System.Collections.Generic.KeyNotFoundException -> None |> Lower
            }) |> pipe4 pName pDescription pType pRest


let manyVariables = 
    key "variables" ":" >>. manyRA pVariable variables .>> ws

// END OF VARIABLES PARSER


let searchForVariable (var:string) = 
    if ResizeArray.exists ( fun (variable:Variable) -> 
        match variable.Name with 
        | Name s -> 
            String.Equals(s, var)
        | _ -> false) variables 
    then var
    else
        errorMsg.AppendFormat ("\tError: No such variable definition: Variable({0}) \r\n", var) |> ignore
        var

// ARITHMETIC EXPRESSION PARSER

// search for numeric variable, if it is not numeric, check if it is a variable at all
let bracketedVariable = 
    betweenSquare pAnyString
    >>= fun a -> 
        if isNumeric a then
            preturn (a |> Variable)
        else
            match isVariable a with
            | Some _ -> fail (sprintf "Variable [%s] is not numeric type" a)
            | _ -> fail (sprintf "Variable [%s] doesn't exists" a)


// ustalenie kolejności wykonywania działań
let pExpr = new OperatorPrecedenceParser<Expr,unit,unit>()

let expr = pExpr.ExpressionParser

// parses logarithm functions
// "Recursive descent parsers can't parse left-recursive grammars directly. So you'll have to refactor the grammar to avoid the left-recursion"
let pLog = strCI_ws "log" >>. pipe2 expr expr (fun expr1 expr2 -> Expr("log", expr1, expr2)) .>> ws
// for absolute numbers
let pAbs = between (str_ws "|") (str_ws "|") expr
let pConstants e = (strCI_ws "PI" >>% Const Math.PI <|> (strCI_ws "E" >>% Const Math.E)) e
let pChoiceMembership s = (strCI_ws "choice" >>. (pIfTested (fun str -> isChoice str)  "Incorrect choice name" pAnyString) |>> ChoiceMembership .>> ws) s
let pFrame = 
    let pFramePath = (attempt(betweenQuotations pSentence |>> Expr.PathForFrame) <|> (betweenSquare pAnyString |>> Expr.Variable) ) .>> str_ws ","
    let pColumn = (attempt( expr .>> optional(str ",") .>> ws)) 
    strCI "read" >>. parentheses(pipe3 pFramePath (expr .>> ws .>> str_ws ",") pColumn (fun path row column -> Expr.ReadFromCSV(path, row, column)))

pExpr.TermParser <- choice[ number |>> Expr.Const
                            pConstants
                            parentheses expr
                            pAbs |>> fun x -> Prefix("+",x)
                            pLog
                            pChoiceMembership
                            pFrame
                            bracketedVariable ]

// definiowanie operatorów, w argumentach, kolejno:
// typ operatora, odpowiadający mu ciąg znaków, pomijanie białych znaków (zawsze "włączone"), kolejność wykonywania działania, 
// łączność (jeśli jest algebraiczna jak przy dodawaniu lub odejmowaniu, podaje się lewą), funkcja odpowiadająca operatorowi

pExpr.AddOperator(InfixOperator("+", spaces, 1, Associativity.Left, fun a b ->  Expr("+", a, b)) )
pExpr.AddOperator(InfixOperator("-", spaces, 1, Associativity.Left, fun a b -> Expr("-", a, b)) )
pExpr.AddOperator(InfixOperator("*", spaces, 2, Associativity.Left, fun a b -> Expr("*", a, b)) )
pExpr.AddOperator(InfixOperator("/", spaces, 2, Associativity.Left, fun a b -> Expr("/", a, b)) )
pExpr.AddOperator(InfixOperator("%", spaces, 2, Associativity.Left, fun a b -> Expr("modulo", a, b)))
pExpr.AddOperator(InfixOperator("^", spaces, 3, Associativity.Right, fun a b -> Expr("^", a, b)) )
pExpr.AddOperator(PrefixOperator("-", spaces, 4, true,  fun x -> Prefix("-", x)) )
pExpr.AddOperator(PrefixOperator("+", spaces, 4, true, fun x -> Prefix("+", x)))
pExpr.AddOperator(PrefixOperator("ln", spaces, 4, true, fun x -> Prefix("ln", x)))
pExpr.AddOperator(PrefixOperator("exp", spaces, 4, true, fun x -> Prefix("exp", x)))
pExpr.AddOperator(PrefixOperator("sin", spaces, 4, true, fun x -> Prefix("sin", x)))
pExpr.AddOperator(PrefixOperator("cos", spaces, 4, true, fun x -> Prefix("cos", x)))
pExpr.AddOperator(PrefixOperator("tan", spaces, 4, true, fun x -> Prefix("tan", x)))
pExpr.AddOperator(PrefixOperator("cot", spaces, 4, true, fun x -> Prefix("cot", x)))
pExpr.AddOperator(PrefixOperator("asin", spaces, 4, true, fun x -> Prefix("asin", x)))
pExpr.AddOperator(PrefixOperator("acos", spaces, 4, true, fun x -> Prefix("acos", x)))
pExpr.AddOperator(PrefixOperator("atan", spaces, 4, true, fun x -> Prefix("atan", x)))
pExpr.AddOperator(PrefixOperator("acot", spaces, 4, true, fun x -> Prefix("acot", x)))
pExpr.AddOperator(PrefixOperator("sinh", spaces, 4, true, fun x -> Prefix("sinh", x)))
pExpr.AddOperator(PrefixOperator("cosh", spaces, 4, true, fun x -> Prefix("cosh", x)))
pExpr.AddOperator(PrefixOperator("tanh", spaces, 4, true, fun x -> Prefix("tanh", x)))
pExpr.AddOperator(PrefixOperator("sqrt", spaces, 4, true, fun x -> Prefix("sqrt", x)))
pExpr.AddOperator(PrefixOperator("cbrt", spaces, 4, true, fun x -> Prefix("cbrt", x)))
pExpr.AddOperator(PostfixOperator("!", spaces, 4, true, fun x -> Prefix("!", x) ))

// finished parser for expression
let pCompleteExpression = expr .>> ws 

// END OF ARITHMETIC EXPRESSION PARSER


// BOOLEAN EXPRESSIONS PARSER

let pBool = new OperatorPrecedenceParser<BoolExpr,unit,unit>()

let boolTerm = pBool.ExpressionParser

// component functions
let pTrue = strCI_ws "TRUE" >>% LogicalConst true
let pFalse = strCI_ws "FALSE" >>% LogicalConst false
let pTF = pTrue <|> pFalse .>> ws

// parsing Qualifier comparisons from syntax rules
let pQualifierComparison = //returns list of possible states of qualifier
    let sentenceSymbols = "_ ?!.,-=+-*%$/~<>:;\'\\()[]ąęćłńóśżź"
    let pQuestion = 
        many1CharsTill (letter <|> digit <|> anyOf sentenceSymbols) ( lookAhead (strCI_ws "NOT" <|> str_ws "{") ) 
        |>> fun str -> str.Trim(',',' ')
    let pNot = ws >>. strCI_ws "NOT" >>% true <|>% false .>> ws 
    let pEnums = sepBy1 (betweenCurly pSentence) (str_ws "OR") <??> "expected sequence of {SOMETHING} separated by OR with possible NOT at the beginning" .>> ws
    
    (fun question isNot possibleValues -> 
        match findQualifierByQuestion question with // search for qualifier
        | Some qualifier when possibleValues |> List.forall (fun posValue -> List.contains posValue qualifier.unwrapEnums) -> // guard for non-existing enums
            if isNot then // case of NOT before expression
                let alterValues = List.except possibleValues qualifier.unwrapEnums
                question, [ for value in alterValues do yield value ]
            else
                question, [ for value in possibleValues do yield value ]
        | Some qualifier ->  failwith (sprintf "Possibly incorrect values of qualifier: \n%A\n in {...} brackets" qualifier)
        | _ -> failwith "Reference to non-existing qualifier value"
    ) |> pipe3 pQuestion pNot pEnums 

let pChoiceComparison s = (strCI_ws "choice" >>. pAnyString .>> ws) s

// String comparison parser
let pStringLiteralOrVar = 
    (betweenQuotations pSentence |>> StringConst) <|> //function checks if [VARIABLE] is string
    (betweenSquare pAnyString >>= (fun sVar -> if isString sVar then preturn sVar else fail "Not a string variable" ) |>> StringVar) 
let pStringComparison s = s |> ((pStringLiteralOrVar .>> ws_str "=" .>>. pStringLiteralOrVar .>> ws ) |>> BoolExpr.StringComparison)


pBool.TermParser <- choice [attempt pStringComparison
                            attempt pChoiceComparison |>> ChoiceComparison
                            attempt (pCompleteExpression |>> Expression |>> BoolLeaf) 
                            attempt (parentheses boolTerm .>> ws) 
                            attempt pTF
                            pQualifierComparison |>> QualifierComparison ]

pBool.AddOperator(InfixOperator("and:", spaces, 2, Associativity.Left, fun a b -> Logical("AND", a, b)))
pBool.AddOperator(InfixOperator("AND", spaces, 2, Associativity.Left, fun a b -> Logical("AND", a, b)))
pBool.AddOperator(InfixOperator("OR", spaces, 1, Associativity.Left, fun a b -> Logical("OR", a, b)))
pBool.AddOperator(InfixOperator("=", spaces, 3, Associativity.Left, fun a b -> Comparison("=", a, b)))
pBool.AddOperator(InfixOperator("==", spaces, 3, Associativity.Left, fun a b -> Comparison("=", a, b)))
pBool.AddOperator(InfixOperator(">", spaces, 4, Associativity.Left, fun a b -> Comparison(">", a, b)))
pBool.AddOperator(InfixOperator(">=", spaces, 4, Associativity.Left, fun a b -> Comparison(">=", a, b)))
pBool.AddOperator(InfixOperator("<", spaces, 4, Associativity.Left, fun a b -> Comparison("<", a, b)))
pBool.AddOperator(InfixOperator("<=", spaces, 4, Associativity.Left, fun a b -> Comparison("<=", a, b)))
pBool.AddOperator(PrefixOperator("NOT", spaces, 5, true, fun x -> Not(x)))

let boolExpression = boolTerm .>> ws

// END OF BOOLEAN EXPRESSION PARSER    


// RULE PARSER DEFINITION

let pRule =
    let assinging = strCI_ws "is" >>. strCI_ws "given" >>. strCI_ws "the" >>. strCI_ws "value"
    let pRuleName = opt (key "rule" ":" >>. pAnyString .>> ws)
    let pIf = key "IF" ":" >>. boolExpression .>> ws

    // constituents of pThen parser
    let pChoiceValue = pfloat |>> (fun choiceConf -> if -1. <= choiceConf && choiceConf <= 1. then choiceConf else failwith "Incorrect choice confidence value. Must be between -1 and 1")
    let pChoice = 
        (str_ws ">" >>? pIfTested isChoice "Choice is not defined" pAnyString .>>. (ws_str "-" 
        >>. choice [ attempt(strCI_ws "confidence")
                     attempt(strCI_ws "conf.")
                     attempt(strCI_ws "probability")
                     (strCI_ws "prob.") ] 
        >>. (attempt(ws_str "=") <|> (str_ws ":")) >>. pChoiceValue .>> ws)) |>> AssignChoice
    let pReport = strCI_ws "X>" >>? optional(str_ws "\"") >>? pSentence |>> Report .>> optional(str_ws "\"")
    let pQuoted = betweenQuotations pSentence//for string literal
    let pStringVar = (pIfTested isString "string variable is not declared" (betweenSquare pAnyString)) |>> StringVar
    let pStringExpr = sepBy1 (attempt pStringVar <|> (pQuoted |>> StringConst)) (strCI_ws "+")
    let pAssignVariable = 
        betweenSquare pAnyString >>=? fun a -> 
            if isString a then 
                assinging >>? pStringExpr .>> ws >>= fun b -> preturn (a, b) |>> AssignString 
            elif isNumeric a then
                assinging >>? (attempt(pCompleteExpression) <|> (strCI_ws "\"_\"" >>% (Const 0.)) ) >>= fun b -> preturn (a, b) |>> AssignNumeric
            else
                fail "Expected variable assignment. Given var is not declared as string nor numeric" 
    let pAssignQualifier = 
        (pSentence |>> trim) >>=? fun qualifierKey -> 
            if isQualifier qualifierKey then
                sepEndBy1 (betweenCurly(pSentence |>> trim) .>>. opt(str_ws "-" 
                >>. choice [ attempt(strCI_ws "confidence")
                             attempt(strCI_ws "conf.")
                             attempt(strCI_ws "probability")
                             (strCI_ws "prob.") ] 
                >>. choice [ attempt(str_ws "="); (str_ws ":")]
                >>. pfloat .>> ws) ) (optional(str_ws ",")) >>= fun b -> 
                    if isEnum qualifierKey (fst <| List.unzip b) then 
                        let valuesWithProperConfidence =
                            [ for qualifierValue in b do 
                                 match snd(qualifierValue) with
                                 | Some confidence -> yield (fst(qualifierValue), confidence)
                                 | None -> yield (fst(qualifierValue), 1.)]
                        preturn(qualifierKey,valuesWithProperConfidence) |>> AssignQualifier
                    else 
                        match ResizeArray.tryFind (fun (q:Qualifier) -> q.unwrapQuestion = qualifierKey) qualifiers with // to find list of possible enums for qualifier
                                    | Some q -> q.sprintfEnums
                                    | None -> "(no such qualifier finded)"
                        |> sprintf """Possibly incorrect enumeration in: %A\r\nSee in: %A\r\n""" b
                        |> fail
            else 
                fail "No such qualifier declared"

    let pClear = 
        strCI_ws "X>" >>? strCI_ws "clear" >>? parentheses(
            choice [attempt(strCI_ws "Q" >>. sepEndBy1 ((pIfTested isQualifierName "There should be qualifier name in square brackets" (betweenSquare pAnyString)) 
                    |>> Terminal.QualifierName) (str_ws ","))
                    attempt(strCI_ws "S" >>. sepEndBy1 ((pIfTested isString "There should be string variable in square brackets" (betweenSquare pAnyString)) 
                    |>> Terminal.StringVar) (str_ws ","))
                    attempt(strCI_ws "N" >>. sepEndBy1 ((pIfTested isNumeric "There should be numeric variable in square brackets" (betweenSquare pAnyString) ) 
                    |>> Terminal.NumericVar) (str_ws ","))
                    (strCI_ws "C" >>. sepEndBy1 ((pIfTested isChoice "There should be choice name" pAnyString ) |>> Terminal.ChoiceVar) (str_ws ","))]) |>> Clear

    let pSave = 
        let pPath = (attempt(betweenSquare (pIfTested (fun str -> isString str) (sprintf "Expected string variable. Incorrect one inputed." ) pAnyString) |>> StringVar)
            <|> (betweenQuotations pSentence |>> StringConst)) .>> str_ws ","
        let pRowColumn = (pCompleteExpression .>> str_ws ",") 
        let pToSave = 
            attempt( (pIfTested isQualifierName "There should be qualifier name in square brackets" (betweenSquare pAnyString)) |>> QualifierName) 
            <|> ( (pIfTested isNumeric "There should be numeric variable in square brackets" (betweenSquare pAnyString) ) |>> NumericVar)
        strCI_ws "X>" >>? strCI_ws "save" >>? parentheses( pipe4 pPath pRowColumn pRowColumn pToSave (fun path row column toSave -> Save(path,row,column,toSave)))

    let pDefuzzify = (strCI_ws "X>" >>? strCI_ws "defuzzify") >>% Defuzzify

    let pDelay = 
        let pQualifier = (pIfTested isQualifierName "Expected qualifier name in square brackets" (betweenSquare pAnyString)) |>> QualifierName
        let pVarToDelay = strCI_ws "Q" >>. pQualifier
        strCI_ws "X>" >>? strCI_ws "delay" >>? parentheses( (pipe2 (pVarToDelay .>> strCI_ws ",") pQualifier (fun q1 q2 -> Delay(q1,q2) ))) .>> ws

    let pRead = 
        let pPath = (attempt(betweenSquare (pIfTested (fun str -> isString str) (sprintf "Expected string variable. Incorrect one inputed." ) pAnyString) |>> StringVar)
                <|> (betweenQuotations pSentence |>> StringConst)) .>> str_ws ","
        let pRowColumn = (pCompleteExpression .>> str_ws ",") 
        let toRead = 
            attempt( (pIfTested isQualifierName "There should be qualifier name in square brackets" (betweenSquare pAnyString)) |>> QualifierName) 
            <|> ( (pIfTested isNumeric "There should be numeric variable in square brackets" (betweenSquare pAnyString) ) |>> NumericVar)
        strCI_ws "X>" >>? strCI_ws "read" >>? parentheses( pipe4 pPath pRowColumn pRowColumn toRead (fun path row column toRead -> Read(path,row,column,toRead)))
        
    let pOperations =  sepBy1 ( choice [pChoice
                                        pDelay
                                        pClear
                                        pRead
                                        pDefuzzify
                                        pSave
                                        pReport
                                        pAssignVariable
                                        pAssignQualifier] ) (key "and" ":") 
    let pThen = key "THEN" ":" >>. pOperations .>> ws
    let pElse = opt( pIfTested (fun a -> basicInfo.Probability <> (ProbabilityMode.Fuzzy |> Probability)) "The else part isn't allowed in fuzzy mode" (key "ELSE" ":" >>. pOperations .>> ws) )
    
    ( fun b c d e ->
        {
            Number = 0 // will be overwritten to the correct index
            Name = b
            IfClauses = c
            ThenStatements = d
            ElseStatements = e
            State = Unverified
        })
    |> pipe4 pRuleName pIf pThen pElse

let manyRules =
    key "rules" ":" >>. (manyRA pRule rules) |>> (fun rRA -> rules <- indexElementsOfRules rRA; rules).>> ws

// END OF RULE PARSER DEFINITION


// COMPLETE PARSER

let parse =
    ws 
    >>. optional(str_ws "LIST ONLY") 
    >>. 
        pBasic         >>= fun a -> 
        basicInfo <- a
        manyQualifiers >>= fun b ->
        manyChoices    >>= fun c ->
        manyVariables  >>= fun d -> 
        manyRules      >>= fun e -> preturn ((fun a b c d e -> [|a |> Basic; b |> Qualifiers; c |> Choices; d |> Variables; e |> Rules|]) basicInfo b c d e)
    .>> ws
    .>> eof

// END OF COMPLETE PARSER



////////// INTERPRETER 
(* 
Mechanizm wnioskowania:
*)

let mutable evalBool = declare<BoolExpr -> float>
let mutable eval = declare<Expr -> float>
let mutable lookUpString = declare<string -> string>
let mutable lookUpQualifier = declare<string -> string list -> float>
let mutable lookUpNumeric = declare<string -> float>



// PROMPT functions - questioning user for values

let rec promptChoice name = 
    if isChoice name then
        let (Some choiceRA) = ResizeArray.tryFind (fun (tryChoice:Choice) -> match tryChoice with {Name = nameFound} -> nameFound = name | _ -> false) choices
        let userInput = prompt (sprintf "Choice %s is not defined. Please, set it by defining truthness value (0-1):" name)
        match (ws >>. pfloat) (new CharStream<String>(userInput, 0, userInput.Length)) with
        | output when output.Status = Ok && 0. <= output.Result && output.Result <= 1. -> 
            choiceDict.Item name <- output.Result |> Some
        | output when output.Status = Ok -> 
            printfn "Incorrect choice %A truthness value. Must be a number between 0 and 1." name
            promptChoice name
        | _ ->
            printfn "Incorrect choice %A truthness value. Must be a number between 0 and 1." name
            promptChoice name
            
    else failwith (sprintf "Incorrect choice name - %A" name)

let rec recPrompt var =
    if isString var then 
        let (Some variableRA) = ResizeArray.tryFind (fun (v:Variable) -> if v.unwrapName = var then true else false) variables
        let userInput = prompt (sprintf "String variable %s - %s in %s dictionary is not defined.\nPlease, set it:" var variableRA.unwrapDescription "string") 
        match (ws >>. pAnyString) (new CharStream<String>(userInput, 0, userInput.Length)) with
        | output when output.Status = Ok -> stringVariableDict.Item(var) <- output.Result |> Some
        | _ -> 
            printfn "Incorrect string. Expected ASCII letters, digits or '_' symbol"
            recPrompt var
    elif isNumeric var then 
        let (Some variableRA) = ResizeArray.tryFind (fun (v:Variable) -> if v.unwrapName = var then true else false) variables
        let userMessage = sprintf "Numeric variable %s - %s in numeric dictionary is not defined. It should be in range:\nUpper limit = %A\nLower limit = %A\nPlease, set it:" var variableRA.unwrapDescription variableRA.UpperLimit variableRA.LowerLimit
        let userInput = prompt userMessage 
        match ((ws >>. expr) (new CharStream<unit>(userInput, 0, userInput.Length))) with 
        | output when output.Status = Ok -> 
            let evaluated = eval output.Result
            if inRange var evaluated then 
                numericVariableDict.Item(var) <- evaluated |> Some
            else 
                printfn "Variable %s is not in predefined range: %A" var (ResizeArray.find (fun (v:Variable) -> v.unwrapName = var) variables).getRange
                recPrompt var           
        | _ -> 
            printfn "Incorrect expression"
            recPrompt var
    (*  Should show list od enumerations to the user (with number annotations) 
        then take comma separated list of this numers from the user input 
        and set corresponding enumerations to dict *)
    else
        failwith "No such variable in strings nor numerics dictionaries"

let rec promptQualifier key = 
    if isQualifier key then 
        let q = qualifiers.Find(fun q -> key = q.unwrapQuestion)
        printfn "\r\nQualifier %i with body (question) \"%s\" is not defined.\nPlease, choose from the list: \r\n" q.Number key
        q.printfEnums
        let userInput = prompt (sprintf "\r\ntyping chosen numbers bodies (without any brackets and quotations) separated by comma ','") 
        let pEnums s = ( ws >>. sepEndBy (pint32 .>> optional (ws_str ")")) (str_ws ",") ) s
        match pEnums (new CharStream<String>(userInput, 0, userInput.Length)) with // parsing input in order to get index of enums to assign them
        | output when output.Status = Ok -> 
            if output.Result = [] then
                qualifierDict.Item key <- Some []
            elif output.Result |> List.forall (fun num -> num > 0  && num <= q.Enumerations.Length) then
                qualifierDict.Item key <- 
                    [ for num in output.Result do yield (q.unwrapEnums.Item (num - 1)),1.] //each user defined enum has default membership level equal to 1.0
                    |> Some
            else
                printfn "Incorrect assignment to qualifier. \r\n%A \r\nShould be: \r\n1 \r\nOR \r\n9, 4" output.Error
                promptQualifier key
        | output -> 
            printfn "Incorrect assignment to qualifier. \r\n%A \r\nShould be: \r\n1 \r\nOR \r\n9, 4" output.Error
            promptQualifier key
    else failwith (sprintf "No such qualifier: %A" key)

// 1. Try to get value directly from dict
// 2. Then, try to get it from the "then" part of rule
//    - If it doesn't 
// 3. Still trying get it, ask user directly

// END OF PROMPT FUNCTIONS


// DEFUZZIFING FUNCTION

let defuzzifyQualifiers () = 
    // not every fuzzified qualifier has to be defuzzified
    // defuzzification is performed using height method
    let qualifiersForDefuzzification = ResizeArray.filter (function qualifier -> qualifier.Defuzzify.IsSome) qualifiers 
    let outputValues = [
        for qualifier in qualifiersForDefuzzification do
            let key = qualifier.unwrapQuestion
            match qualifierDict.TryGetValue (key) with
            | true, Some membershipFunctions -> 
                yield key, (defuzzify (qualifier) membershipFunctions)
            | _ -> failwith (sprintf "Incorrect state: %A \nIs that list empty? \n" qualifiersForDefuzzification)
    ]

    for value in outputValues do
        match ResizeArray.tryFind (function (q:Qualifier) -> q.unwrapQuestion = (fst value) && q.Defuzzify.IsSome) qualifiers with
        | Some q -> 
            let (Some defuzzifiedVar) = q.Defuzzify // assured by finding above
            match ResizeArray.tryFind (function (v:Variable) -> v.unwrapName = defuzzifiedVar) variables with
            | Some vRA ->
                let range = vRA.getRange
                let newValue = snd value
                numericVariableDict.Item defuzzifiedVar <- 
                    if newValue > snd(range) then snd(range) |> Some
                    elif newValue < fst(range) then fst(range) |> Some
                    else newValue |> Some
            | None -> failwith (sprintf "Cannot find variable %A in memory" defuzzifiedVar)
        | None -> failwith (sprintf "No variable to defuzzify value %A of qualifier %A. Incorrect state." (snd value) (fst value))
    outputValues

// END OF DEFUZZIFING FUNCTION


// FILTERING rules that have assignent to desired variable

let rulesWithAssignment key (stateOfRules: RuleValue -> bool) = 
    rules 
    |> ResizeArray.filter (fun r -> 
        stateOfRules(r.State) 
        &&
        r.ThenStatements 
        |> List.exists (function
            | AssignNumeric(var, _) when isNumeric key && var = key -> true
            | AssignString(var, _) when isString key && var = key -> true
            | AssignQualifier(q, _) when isQualifier key && q = key -> true
            | AssignChoice(choice, _) when isChoice choice && choice = key -> true
            | _ -> false )
        ||
        if r.ElseStatements.IsNone
        then []
        else r.ElseStatements.Value
        |> List.exists (function
            | AssignNumeric(var, _) when isNumeric key && var = key -> true
            | AssignString(var, _) when isString key && var = key -> true
            | AssignQualifier(q, _) when isQualifier key && q = key -> true
            | AssignChoice(choice, _) when isChoice choice && choice = key -> true
            | _ -> false ))
    |> ResizeArray.toList

let unverifiedRulesWithAssignment name = rulesWithAssignment name (function RuleValue.Unverified -> true | _ -> false)

let truOrFalseRulesWithAssignment name = rulesWithAssignment name (function RuleValue.True _ | RuleValue.False -> true | _ -> false)

// END OF FILTERING FUNCTION


// EXECUTING rules - its then or else parts

let rec evalStringExpression (acc:string) = function //implicit one argument
    | [] -> acc
    | (StringConst literal) :: rest -> 
        evalStringExpression (acc + literal) rest
    | (StringVar var) :: rest -> 
        if isString var then
            evalStringExpression (acc + lookUpString var) rest
        else
            failwith "No such string declared"
    | _ -> failwith "Illegal state"

let evalOperation operation firingLevel = 
    match operation with
    | AssignString(var, str) -> 
        if isString var then
            stringVariableDict.Item var <- evalStringExpression "" str |> Some
        else failwith (sprintf "No such string variable declared: %A" var)
    | AssignNumeric(var, expr) -> 
        if isNumeric var then
            match eval expr, ResizeArray.tryFind (function (v:Variable) -> v.unwrapName = var) variables with
            | nan, _ when nan = System.Double.NaN -> 
                printfn "Expression %A is incorrect at some point. Check if the output is within domain range" expr
                recPrompt var
            | correctNumber, Some vRA -> 
                let range = vRA.getRange
                numericVariableDict.Item var <- 
                    if correctNumber > snd(range) then snd(range) |> Some
                    elif correctNumber < fst(range) then fst(range) |> Some
                    else correctNumber |> Some
            | _ -> failwith (sprintf "Cannot find variable %A in memory" var)
        else failwith (sprintf "No such numeric variable declared: %A" var)
    | AssignQualifier(q, vl) -> 
        match qualifierDict.TryGetValue q with
        | true, Some currentValues -> 
            qualifierDict.Item q <-  
                [ 
                    let enums = (ResizeArray.find (fun (qualifier:Qualifier) -> qualifier.unwrapQuestion = q) qualifiers).unwrapEnums
                    for enum in enums do
                        match List.tryFind (function valueFromAssignment -> enum = fst(valueFromAssignment)) vl, List.tryFind (function current -> enum = fst(current)) currentValues with
                        | Some valueFromAssignment, Some currentValue -> 
                            let assignmentImplication = fuzzyAND firingLevel (snd valueFromAssignment)
                            if basicInfo.OrCurrentValue = (OrCurrent true) 
                            then yield enum, (fuzzyOR assignmentImplication (snd currentValue)) 
                            else yield enum, assignmentImplication
                        | Some valueFromAssignment, None -> 
                            let assignmentImplication = fuzzyAND firingLevel (snd valueFromAssignment)
                            yield enum, assignmentImplication
                        | None, Some currentValue -> 
                            yield currentValue
                        | None, None -> yield enum, 0.
                ] 
                |> noneIfEmpty 
        | true, None -> 
            qualifierDict.Item q <- 
                [ for valueFromAssignment in vl do yield fst(valueFromAssignment), ( fuzzyAND firingLevel (snd valueFromAssignment) ) ]
                |> noneIfEmpty
        | _ -> failwith (sprintf "No such qualifier declared: %A" q)
    | AssignChoice(choice, valueFromAssignment) -> 
        match choiceDict.TryGetValue choice with
        | true, Some currentConf -> 
            let assignmentImplication = fuzzyAND firingLevel valueFromAssignment
            if basicInfo.OrCurrentValue = (OrCurrent true) 
            then choiceDict.Item choice <- Some(fuzzyOR currentConf assignmentImplication) //currentConf = currentConf + (ifFiringLevel * explicitValue)
            else choiceDict.Item choice <- Some assignmentImplication
            printfn "\r\nThe chosen choice is: %s" choice |> ignore
        | true, None -> choiceDict.Item choice <- Some(fuzzyAND firingLevel valueFromAssignment) // currentConf = ifFiringLevel * explicitValue
        | _ -> failwith (sprintf "%A - no such choice declared" choice)
    | Clear(terminals) -> 
        for terminal in terminals do
            match terminal with
            | Terminal.QualifierName name -> 
                match ResizeArray.tryFind (function (q:Qualifier) -> q.unwrapName = name) qualifiers with
                | Some q -> qualifierDict.Item (q.unwrapQuestion) <- None
                | None -> failwith (sprintf "There is no qualifier with name: %A" name)
            | NumericVar num ->  if numericVariableDict.ContainsKey num then numericVariableDict.Item num <- None
            | StringVar str -> if stringVariableDict.ContainsKey str then stringVariableDict.Item str <- None
            | ChoiceVar c -> if choiceDict.ContainsKey c then choiceDict.Item c <- None
            | _ -> failwith "incorrect state. Should be Q, N, S or C"
    | Defuzzify -> defuzzifyQualifiers () |> ignore
    | Save(path, row, column, toSave) -> 
        let newPath =
            match path with
                | StringConst pathStr -> pathStr
                | StringVar var -> (stringVariableDict.Item var).Value
                | _ -> failwith (sprintf "Incorrect state. The path for saving is not recognized as string variable nor string literal")
        match toSave with
        | Terminal.NumericVar num ->
            CSVset newPath (eval row) (eval column) (lookUpNumeric num)
        | QualifierName name -> 
            match ResizeArray.tryFind (function (q:Qualifier) -> q.unwrapName = name) qualifiers with
                | Some q -> 
                    match qualifierDict.Item (q.unwrapQuestion), q.unwrapEnums with
                    | Some qualifierValues, enums -> 
                        for i = 0 to enums.Length - 1 do
                            match List.tryFind (fun value -> fst(value) = enums.[i]) qualifierValues with
                            | Some value -> CSVset newPath (eval row) (eval column + (float i)) (Y(value))
                            | None -> CSVset newPath (eval row) (eval column + (float i)) 0.
                    | None, enums -> 
                        for i = 0 to enums.Length - 1 do
                            CSVset newPath (eval row) (eval column + (float i)) 0.
                | None -> failwith (sprintf "There is no qualifier with name: %A" name)
    | Read(path, row, column, toRead) ->
        let newPath =
            match path with
                | StringConst pathStr -> pathStr
                | StringVar var -> (stringVariableDict.Item var).Value
                | _ -> failwith (sprintf "Incorrect state. The path for saving is not recognized as string variable nor string literal")
        match toRead with
        | Terminal.NumericVar num ->
            numericVariableDict.Item num <- Some( CSVget newPath (eval row) (eval column) ) 
        | QualifierName name -> 
            match ResizeArray.tryFind (function (q:Qualifier) -> q.unwrapName = name) qualifiers with
                | Some q -> 
                    let enums = q.unwrapEnums
                    let newValues = [
                        for i = 0 to enums.Length - 1 do
                            yield enums.[i], CSVget newPath (eval row) (eval column + (float i))
                    ]
                    qualifierDict.Item q.unwrapQuestion <- newValues |> noneIfEmpty
                | None -> failwith (sprintf "There is no qualifier with name: %A" name)
        | _ -> failwith "Incorrect state. Expected name of qualifier or numeric variable. In both cases in square brackets."
    | Delay(QualifierName q1, QualifierName q2) -> 
        match ResizeArray.tryFind (function (q:Qualifier) -> q.unwrapName = q1) qualifiers,ResizeArray.tryFind (function (q:Qualifier) -> q.unwrapName = q2) qualifiers with
        | Some q1, Some q2 -> 
            qualifierDict.Item q2.unwrapQuestion <- qualifierDict.Item q1.unwrapQuestion
        | _, Some q2 -> failwith  "Incorrect state. Qualifier to be delayed doesn't exists" 
        | Some q1, _ -> failwith  "Incorrect state. Qualifier of pervious value doesn't exists" 
        | _ , _ -> failwith  "Incorrect state. Qualifier of pervious value doesn't exists" 
    | Report rep -> commandLog.AppendFormat ("\r\nReport: {0}", rep) |> ignore
        

let evalThen r firingLevel = 
    for operation in r.ThenStatements do
        evalOperation operation firingLevel

let evalElse r firingLevel = 
    match r.ElseStatements with
    | Some elses ->
        for operation in elses do
            evalOperation operation firingLevel
    | None -> ()            
    
let interpretRule (r:Rule) = 
    let (FuzzyThreshold fuzzyThreshold) = basicInfo.FuzzyThreshold
    let ifFiringLevel = evalBool r.IfClauses
    if ifFiringLevel > fuzzyThreshold then
        evalThen r ifFiringLevel
        if basicInfo.Derivation <> Derivation(Derivation.AllRulesUsed) then setRule r ifFiringLevel
    else
        evalElse r ifFiringLevel // if rule is not on "Selected" list
        if basicInfo.Derivation <> Derivation(Derivation.AllRulesUsed) then setRule r ifFiringLevel
               
let executeSelectedRules (rl: Rule list) = List.iter (interpretRule) rl; rl

// "rules" argument from rulesWithQualifierAssignment function
let executeRules (key:string) (rl:Rule list) = 
    if rl.IsEmpty then 
        // no rule with assignment to that qualifier or rule has been checked but qualifier was set to "None" somehow
        errorMsg.AppendFormat ("No rule for this variable - '{0}' \nin the collection of unverified rules\r\n", key) |> ignore 
        errorMsg.AppendLine ("List of rules with assignment to that qualifier and executed (verified): ") |> ignore 
        truOrFalseRulesWithAssignment key
        |> List.map (fun r -> ruleFormat r)
        |> errorMsg.Append |> ignore // prints list of verified rules with eligible assignment to the error messages
        []
    else 
        executeSelectedRules rl

// END OF EXECUTING RULES


// LOOKUP functions - finished getter which runs suitable rules or questions the user for values 

let lookUpChoice name = 
    match choiceDict.TryGetValue name with 
    | true, Some value -> value
    | true, None -> 
        let selectedRules = unverifiedRulesWithAssignment name
        executeRules name selectedRules |> ignore
        match choiceDict.TryGetValue name with
        | true, Some value -> value
        | true, None -> 
            promptChoice name
            match choiceDict.TryGetValue name with | true, Some value -> value | _ -> failwith "Incorrect state."
        | _ ->  failwith (sprintf "No such choice - %A" name)
    | _ -> failwith (sprintf "No such choice - %A" name)

lookUpNumeric <-
    fun key ->
        match numericVariableDict.TryGetValue(key), ResizeArray.tryFind (function (q: Qualifier) -> match q.Defuzzify with Some numToDefuzz -> numToDefuzz = key | _ -> false) qualifiers with
        | (true, _), Some q -> 
            let qualifierKey = q.unwrapQuestion
            match qualifierDict.TryGetValue (qualifierKey) with
            | true, Some values -> 
                let defuzzifiedVal = defuzzify q values
                numericVariableDict.Item key <- Some defuzzifiedVal
                defuzzifiedVal
            | true, None -> 
                lookUpQualifier qualifierKey q.unwrapEnums |> ignore
                let (_, Some(values)) = qualifierDict.TryGetValue (qualifierKey)
                let defuzzifiedVal = defuzzify q values
                numericVariableDict.Item key <- Some defuzzifiedVal
                defuzzifiedVal
            | _ -> failwith "No correspond enumeration in enums dictionary"
        | (true, Some value), _ -> value
        | (true, _), _ ->
            let usedRules = unverifiedRulesWithAssignment key
            executeRules key usedRules |> ignore
            match numericVariableDict.TryGetValue key with
            | true, Some value -> 
                printfn "Numeric variable %s, value: %f, infered from rules:" key value
                for r in usedRules do
                    if r.Name.IsSome
                    then printfn "Rule: %s, nr: %i" r.Name.Value r.Number
                    else printfn "Rule nr: %i" r.Number 
                value
            | true, _ -> 
                printfn "\r\nNo rule for numeric variable '%s' in the collection of unverified rules" key
                recPrompt key
                numericVariableDict.[key].Value
            | _ -> failwith (sprintf "No such numeric variable defined: %s" key)
        | _ -> failwith (sprintf "No such numeric variable defined: %s" key)
            
lookUpString <- 
    fun key -> 
        match stringVariableDict.TryGetValue(key) with
        | true, Some value ->
            value
        | true, _ ->
            let usedRules = unverifiedRulesWithAssignment key 
            executeRules key usedRules |> ignore
            match stringVariableDict.TryGetValue key with
            | true, Some value -> 
                printfn "String variable %s, value: %s, infered from rules:" key value
                for r in usedRules do
                    if r.Name.IsSome
                    then printfn "Rule: %s, nr: %i" r.Name.Value r.Number
                    else printfn "Rule nr: %i" r.Number 
                value
            | true, _ ->
                printfn "\r\nNo rule for string variable '%s' in the collection of unverified rules" key
                recPrompt key
                stringVariableDict.[key].Value
            | _ -> failwith (sprintf "No such string variable defined: %s" key)
        | _ -> failwith (sprintf "No such string variable defined: %s" key)

// END OF LOOKUPS

// ARITHMETIC EXPRESSION INTERPRETER

eval <- 
    fun (x : Expr) ->
        match x with 
        | Expr.Const x -> x
        | Expr.Variable var -> lookUpNumeric var 
        | Prefix("-", x) -> - (eval(x))
        | Expr("+", a, b) -> eval a + eval b
        | Expr("-", a, b) -> eval a - eval b
        | Expr("*", a, b) -> eval a * eval b
        | Expr("/", a, b) -> eval a / eval b
        | Expr("modulo", a, b) -> eval a % eval b
        | Expr("^", a, b) -> Math.Pow(eval a, eval b)
        | ReadFromCSV(path, row, column) -> 
            let finalPath = 
                match path with
                | PathForFrame p -> p
                | Variable var when isString var -> 
                    match stringVariableDict.Item var with
                    | Some v -> v
                    | None -> failwith (sprintf "Incorrect state. String variable %A is not defined" var)
                | _ -> failwith "Incorrect state - in finding path to the CSV file."
            CSVget finalPath (eval row) (eval column)
        | ChoiceMembership str -> lookUpChoice str
        | Prefix("sqrt", x) -> Math.Sqrt (eval x)
        | Prefix("cbrt", x) -> Math.Pow(eval x, 1./3.)
        | Prefix("!", x) -> factorial (eval x)
        | Expr("log", a, b) -> Math.Log (eval b, eval a)
        | Prefix("ln", x) -> Math.Log (eval x)
        | Prefix("exp", x) -> Math.Exp (eval x)
        | Prefix("sin", x) -> Math.Sin (eval x)
        | Prefix("cos", x) -> Math.Cos (eval x)
        | Prefix("tan", x) -> Math.Tan (eval x)
        | Prefix("cot", x) -> 1.0/(Math.Tan (eval x))
        | Prefix("asin", x) -> Math.Asin (eval x)
        | Prefix("acos", x) -> Math.Acos (eval x)
        | Prefix("atan", x) -> Math.Atan (eval x)
        | Prefix("acot", x) -> Math.Atan (1.0/eval x)
        | Prefix("sinh", x) -> Math.Sinh (eval x)
        | Prefix("cosh", x) -> Math.Cosh (eval x)
        | Prefix("tanh", x) -> Math.Tanh (eval x)
        | Prefix("+", x) -> abs (eval(x))
        | _ -> errorMsg.AppendFormat ("Incorrect expression\r\n") |> ignore; nan

// END OF ARITHMETIC EXPRESSION INTERPRETER


// QUALIFIER LOOKUP
// Calculate fuzzy sum of qualifier values truthness in order to set truthness in logic (if part) expression
let rec sumOfFuzzyValues valuesWithSetTruthness (vals:string list) = // It could be done better - without Some _ option in lookUpQualifier
    // find each value saved in qualifierDict (under correct key, ofc) and sum its membership level in fuzzy way
    match vals with
    | [] -> 0. // fuzzy sum of sth and zero outputs sth
    | qualifierValue :: rest -> 
        match valuesWithSetTruthness with
        | [] -> 0.
        | list -> 
            match List.tryFind (function valXmemFunc -> fst(valXmemFunc) = qualifierValue) list with
            | Some valXmemFunc -> snd valXmemFunc
            | _ -> 0.
            |> fuzzyOR (sumOfFuzzyValues valuesWithSetTruthness rest)

lookUpQualifier <-
    fun key vl ->
        match qualifierDict.TryGetValue key with
        | true, _ when ResizeArray.exists (fun q -> q.Fuzzify.IsSome && q.unwrapQuestion = key) qualifiers ->
            match ResizeArray.tryFind (function (q:Qualifier) -> q.unwrapQuestion = key) qualifiers with
            | Some q -> 
                let valuesToSet = fuzzifyVar (q.Enumerations) (lookUpNumeric <| q.Fuzzify.Value)
                qualifierDict.Item key <- Some(valuesToSet) 
                sumOfFuzzyValues valuesToSet vl
            | None -> failwith (sprintf "No such qualifier defined: %A." key)
        | true, Some valuesWithTruthness -> //if it's infered by now so it only checks its value(s)
            sumOfFuzzyValues valuesWithTruthness vl
        | true, None ->
            //for now inferring only by rules not programs nor data base
            //now we don't want to check all the rules with that qualifier assignment in "then" operations - if any rule satisfy conditons new value goes (?)
            //-> consistency check
            let usedRules = unverifiedRulesWithAssignment key // filter rules with assignment to this qualifier in "then"
            executeRules key usedRules |> ignore //repeatedly execute the rules
            match qualifierDict.TryGetValue key with
            | true, Some options -> 
                printfn "Qualifier variable %s, value: %A, infered from rules:" key options
                for r in usedRules do
                    if r.Name.IsSome
                    then printfn "Rule: %s, nr: %i" r.Name.Value r.Number
                    else printfn "Rule nr: %i" r.Number 
                sumOfFuzzyValues options vl
            | true, None -> 
                printfn "\r\nNo rule for qualifier '%A' in the collection of unverified rules" key
                promptQualifier key
                match qualifierDict.TryGetValue key with
                | true, Some options -> 
                    sumOfFuzzyValues options vl
                | _ -> failwith "Still no qualifier variable defined"
            | _ -> failwith "No such qualifier variable defined"
        | _ -> failwith "No such qualifier variable defined"

// END OF QUALIFIER LOOKUP


// LOGIC EXPRESSION INTERPRETER

let exprToBool (leaf:BoolExpr) = 
    match leaf with
    | BoolLeaf l -> 
        match l with
        | Expression e -> eval e 
        | _ -> failwith "Unable to solve this boolean expression leaf"
    | _ -> failwith "Boolean expression input expected"
    
let evalString input = 
    match input with
    | StringConst s -> s
    | StringVar str -> lookUpString str
    | _ -> failwith (sprintf "Illegal state of: %A" input)

evalBool <- fun (x : BoolExpr) ->
    match x with
    | LogicalConst b -> if b then 1. else 0.
    | Not b -> 1. - (evalBool b)
    | Logical("AND", a, b) -> fuzzyAND (evalBool a) (evalBool b)
    | Logical("OR", a, b) -> fuzzyOR (evalBool a) (evalBool b)
    | StringComparison(s1, s2) -> if evalString s1 = evalString s2 then 1. else 0.
    | QualifierComparison(q, vl) -> lookUpQualifier q vl    
    | Comparison("=", a, b) -> if exprToBool a =~ exprToBool b then 1. else 0.
    | Comparison(">=", a, b) -> 
        let a = exprToBool a
        let b = exprToBool b
        if a > b || a =~ b then 1. else 0.
    | Comparison("<=", a, b) -> 
        let a = exprToBool a
        let b = exprToBool b
        if a < b || a =~ b then 1. else 0.
    | Comparison(">", a, b) -> if exprToBool a > exprToBool b then 1. else 0.
    | Comparison("<", a, b) -> if exprToBool a < exprToBool b then 1. else 0.
    | Comparison(_, a, b) -> failwith "Incorrect state. Comparsion operator must be: = (==), <, >, <=, >="
    | ChoiceComparison(_) -> failwith "Incorrect state. Comparison of choice(es) must be in form: choice X (comparison symbol: <, >, <=, >=, =) (expression - variable, choice or constant) "

// END OF LOGIC EXPRESSION INTERPRETER


// PARSE A FILE

printfn ""
// With input text file
inputString
// alternatively with "writeResult" instead of "test"
|> writeResult parse

// END OF PARSE A FILE


// INTERMEDIATE PROGRAM CHECK

match basicInfo.LoopOverVariable with
| LoopOver(Some var) when isNumeric var -> 
    match ResizeArray.tryFind (function (varRA:Variable) -> varRA.unwrapName = var) variables with
    | Some variableRA -> 
        match variableRA.UpperLimit, variableRA.LowerLimit with 
        | Upper(Some _) , _ -> ()
        | _ -> failwith "Variable to iterate over has to have lower and upper limit"
| LoopOver(None) -> ()

//END OF INTERMEDIATE PROGRAM CHECK


// OUTPUT PRINTERS (DEBUG)
let outputPrinters () =
    printfn ""
    printfn " --------- QUALIFIERS --------"
    for item in qualifierDict do
        if item.Value <> None then
            printfn "%s - %A" item.Key item.Value
        else
            printfn "%s - %s" item.Key "NONE"
    printfn ""
    printfn " ----------- STRINGS ---------"
    for item in stringVariableDict do
        if item.Value <> None then
            printfn "%A" item
        else
            printfn "%A - %s" item.Key "NONE"
    printfn ""
    printfn " ---------- NUMERICS ---------"
    for item in numericVariableDict do
        if item.Value <> None then
            printfn "%A" item
        else
            printfn "%A - %s" item.Key "NONE"
    printfn ""
    printfn " ---------- CHOICES ----------"
    for item in choices do
        if item.State <> None then
            printfn "Choice nr: %i - %s. State: %A" item.Number item.Name item.State
    printfn ""


// END OF OUTPUT PRINTERS
 
// PROGRAM EXECUTION FUNCTIONS

let rec ruleNumber () = 
    let ruleNumbers = ws >>. sepEndBy1 (pint32 .>> ws) (optional(ws_str ","))
    let userInput = prompt ("Choose numbers (int) of rules to check, separated by comma ',':")
    match ruleNumbers (new CharStream<String>(userInput, 0, userInput.Length)) with
    | rl when rl.Status = Ok && List.forall (fun num -> num > 0) rl.Result -> 
        let rec filterRules acc = function 
            | [] -> acc
            | r :: rest -> 
                match ResizeArray.tryFind (fun (rule:Rule) -> rule.Number = r) rules with
                | Some rule -> filterRules (rule :: acc) rest
                | None -> 
                    printfn "Rule with index %i has not been declared" r
                    filterRules acc rest
        filterRules [] rl.Result
    | _ ->
        printfn "Probably one or more of the rules index is not within range or the input is empty"
        ruleNumber ()


let executeProgram () =
    let program () = 
        if basicInfo.Derivation = BasicAttribute.Derivation (Derivation.AllRulesUsed) then 
            ResizeArray.toList rules
            |> executeSelectedRules |> ignore
        else executeSelectedRules (ruleNumber()) |> ignore
        outputPrinters ()

    match basicInfo.LoopOverVariable with
    | LoopOver(Some var) -> 
        match numericVariableDict.TryGetValue var with
        | true, Some(iterations) -> 
            for i = 1 to int(iterations) do 
                numericVariableDict.Item var <- (float i) |> Some
                printfn ""
                printfn "************* ITERATION %A *************" i
                printfn "Iterated variable = %A" i
                printfn ""
                program ()
                printfn ""
        | _ -> program ()
    | _ -> program ()
    

// Program execution

executeProgram () 

// END OF PROGRAM EXECUTION FUNCTIONS


// Print all error messages
System.Console.Write errorMsg |> ignore
System.Console.Write commandLog |> ignore




// Prevents instant execution

printfn "Press any key to terminate"
System.Console.ReadKey()
|> ignore



