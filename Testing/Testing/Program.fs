// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FParsec
open System.IO
open System.Text

// Which file to load?
printfn "Demo file is in \"Testing\\Testing\\ path."
printfn "Write the file name (with .txt) for parsing or type ENTER for Demo1.txt "

let inputString = 
    try
        let fileName = System.Console.ReadLine()
        let filePath:string = __SOURCE_DIRECTORY__ + "\\" + fileName
        File.ReadAllText(filePath, Encoding.UTF8)
        
    with
        :? System.IO.DirectoryNotFoundException -> 
            let filePath2:string = __SOURCE_DIRECTORY__ + "\\Demo1.txt"
            File.ReadAllText(filePath2, Encoding.UTF8)



type Qualifier = 
    { 
        Number: int;
        Question: string; 
        Values: string list;
        Name: string option;
    }

type BasicInfo = 
    { 
        Subject: string;
        Author: string;
        Derivation: string;
        Probability: float;
        Threshold: int32;
    }


let ws = spaces 

let str s = skipString s >>. ws

// parses keywords with separator after and optional spaces between them
let key str separator = skipStringCI str >>. ws >>. skipStringCI separator >>. ws

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


let qualifiers = new ResizeArray<Qualifier>();

// pAnystring parses any ASCII letter or digit combination
let letterOrDigit ch = isAsciiLetter ch || isDigit ch || ch = '_'
let pAnyString = manyChars (letter <|> digit) <??> "Expect only ASCII letters or digits" .>> ws

// parses any letter, space or character typical for statement
let pSentence : Parser<string,'u> = manySatisfy (fun ch -> isLetter ch || (isAnyOf " _?!.,-=+-*%$/~<>:;\'\"\\" ch) ) .>> ws

let manyRA p (arr: ResizeArray<_>) =
    // the compiler expands the call to Inline.Many to an optimized sequence parser. Enable to use "many" with resize array rather than list.
    Inline.Many(elementParser = p,
              stateFromFirstElement = (fun x0 ->
                                         arr.Add(x0)
                                         arr),
              foldState = (fun ra x -> ra.Add(x); ra),
              resultFromState = (fun ra -> ra),
              resultForEmptySequence = (fun () -> arr))


let pQualifier = 
    //parses one occurence of qualifier
    let pQualifierNumber = str "/*" >>. key "qualifier" "" >>. pint32 .>> ws
    let pQualifierQuestion = str "Q>" >>. pSentence .>> ws
    let pQualifierValues = many (str "V>" >>. pSentence .>> ws)
    let pQualifierName = key "name" ":" >>. opt pAnyString <|>% None .>> ws 

    ( fun a b c d -> {Number = a; Question = b; Values = c; Name = d} )
    |> pipe4 pQualifierNumber pQualifierQuestion pQualifierValues pQualifierName 
 
let pQualifiers = 
    //parses many qualifiers occuring after keyword
    ws >>. key "qualifiers" ":" >>. manyRA pQualifier qualifiers 

    
let pBasic = 
    let pBasicSubject = key "Subject" ":" >>. pSentence .>> ws
    let pBasicAuthor = key "Author" ":" >>. pSentence .>> ws
    let pBasicDerivation = key "Derivation" ":" >>. pSentence .>> ws
    let pBasicProbability = key "Probability system" ":" >>. pfloat .>> ws
    let pBasicThreshold = key "Display threshold" ":" >>. pint32 .>> ws

    ( fun a b c d e -> {Subject = a; Author = b; Derivation = c; Probability = d; Threshold = e;})
    |> pipe5 pBasicSubject pBasicAuthor pBasicDerivation pBasicProbability pBasicThreshold
    

let parse =
    // finished function
    ws >>. str "LIST ONLY" >>. pBasic .>>. pQualifiers

//"Qualifiers  : \n /* Qualifier 1 \n\nQ> Czy potrzebne jest przecinanie półfabrykatu? \nV> Tak\nV>Nie\nName: ISCUTOFF \n"

printfn ""

// With input text file
inputString
|> test parse

