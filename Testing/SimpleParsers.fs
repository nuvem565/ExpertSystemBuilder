module SimpleParsers

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
    
