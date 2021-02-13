module Printers
open System.Text
open System.IO
open FParsec
open FSharpx.Collections
// custom modules
open TypeDefinitions

// AUXILIARY OUTPUT FUNCTIONS
// Output directly to cmd
let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMessage, _, _) -> printfn "Failure: %s" errorMessage
       
