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

