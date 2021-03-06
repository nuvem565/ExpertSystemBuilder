﻿module SimpleParsers

open FParsec
open System.Collections.Generic

// AUXILIARY PARSER FUNCTIONS

let ws = spaces 
let ws1 = spaces1
let ch c = pchar c
let str s = skipString s
let strCI s = skipStringCI s
let str_ws s = skipString s >>. ws
let strCI_ws s = skipStringCI s >>. ws
let ws_str s = skipStringCI s .>> ws
let ws_strCI s = skipStringCI s .>> ws

// for parsing comment starting with /* ending with */ or newline
let comment s = str "//*" >>. skipManyTill ((skipChar '*' >>? skipChar '/') <|> skipNewline) s


let removeComments s = 
    let rec skipTillComment (s:char list) = 
        match s with
        | '*' :: '/' :: rest -> rest
        | '\n' :: rest | '\r' :: rest -> '\n' :: '\r' :: rest
        | _ :: rest -> skipTillComment rest
        | [] -> []
    
    let rec searchForComments acc (s:char list) : string = 
        match s with
        | [] -> 
            new System.String(Array.ofList acc |> Array.rev) 
        | '/' :: '*' :: rest -> 
            searchForComments acc (skipTillComment rest)
        | ch :: rest -> searchForComments (ch :: acc) rest

    searchForComments [] (Seq.toList s)
    
// Apply test to the result of parsing by p  or fail non-fatally
/// <summary>Uses parser p and tests the result. Error message (from FParsec) is preceded by parsed value.</summary>
///<param name="test">Function that returns bool</param>
///<returns>The same parser as p</returns>
let pIfTested test error p = 
    p >>= fun s ->  if test s then preturn s else (fail (sprintf "%A - %s" s error))

// for purpose of "between" parser
let openCurly s = skipString "{" s
let closeCurly s = skipString "}" s
let betweenCurly s = between openCurly closeCurly s .>> ws

let openSquare s = skipString "[" s
let closeSquare s = skipString "]" s
let betweenSquare s = between openSquare closeSquare s .>> ws

let quoteMark s = skipString "\"" s
let betweenQuotations s = between quoteMark quoteMark s .>> ws

// parses keywords with separator after and optional spaces between them
let key str separator = skipStringCI str >>. ws >>. skipStringCI separator >>. ws

// pAnystring parses any ASCII letter or digit combination
let letterOrDigit ch = isAsciiLetter ch || isDigit ch || ch = '_'
let pAnyString s = s |> (manyChars (letter <|> digit <|> (pchar '_')) <??> "Expect only ASCII letters, digits or '_'" .>> ws)

// parses any letter, space or typical character for statement
let pSentence s = 
    let symbols = "_ ?!.,-=+-*%$/~<>:;\'\\()[]ąęćłńóśżź"
    s |> 
    ( manySatisfy (fun ch -> isLetter ch || isDigit ch || (isAnyOf symbols ch)) 
    <??> sprintf "Expected any of letter, digit or any of %s" symbols .>> ws )

// no parentheses ()
let pSentence2 s = 
    let symbols = "_ ?!.,-=+-*%$/~<>:;\'\\[]ąęćłńóśżź"
    s |> 
    ( manySatisfy (fun ch -> isLetter ch || isDigit ch || (isAnyOf symbols ch)) 
    <??> sprintf "Expected any of letter, digit or any of %s" symbols .>> ws )


// query for input to parse
let prompt (info:string) = 
    System.Console.WriteLine(info) |> ignore
    System.Console.ReadLine()



// OPERATOR PRECEDENCE PARSER - MATH EXPRESSIONS
// parsowanie liczby - pfloat, usunięcie białych znaków
let number a = (pfloat .>> spaces) a

let pRealNumber s = 
    ( attempt((str_ws "-" >>. strCI "inf") >>% System.Double.MaxValue) <|> 
      attempt(optional(str_ws "+") >>. (strCI "inf") >>% System.Double.MinValue) <|> 
      pfloat ) s


// Argument w formie jawnej (a) zapewnia generyczność kodu (automatyczne uogólnianie)
let parentheses a = between (str_ws "(") (str_ws ")") a



