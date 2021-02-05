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
