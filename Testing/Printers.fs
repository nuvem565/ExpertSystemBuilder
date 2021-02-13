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
       
let operationFormat (op:Operations list) = 
    let sb = new StringBuilder()
    for operation in op do
        sb.Append("\r\n\t\t").Append(operation) |> ignore
    sb.ToString()

let ruleFormat (r:Rule) = 
    let sb = new StringBuilder()
    sb.AppendLine "\r\n{" |> ignore
    sb.AppendFormat("\tRule: {0} - Name: {1}", r.Number, r.Name) |> ignore
    sb.AppendFormat("\r\n\tIF: \r\n\t\t{0}", r.IfClauses) |> ignore
    sb.Append "\r\n\tTHEN:" |> ignore
    sb.AppendFormat ("\r\n\t\t{0}", r.ThenStatements.Head) |> ignore
    for operation in r.ThenStatements.Tail do
        sb.AppendFormat("\r\n\t\tand: {0}", operation) |> ignore
    if r.ElseStatements.IsSome then
        sb.Append "\r\n\tELSE:" |> ignore
        sb.AppendFormat ("\r\n\t\t{0}", r.ElseStatements.Value.Head) |> ignore
        for operation in r.ElseStatements.Value.Tail do
            sb.AppendFormat("\r\n\tand:{0}", operation) |> ignore
    sb.AppendLine "\r\n}" |> ignore
    sb

//let ruleFormat (r:Rule) = 
//    sprintf "\r\n{\r\n\tRule: %d - Name: %A\r\n\tIF: %A\r\n\tTHEN: %A\r\n\tELSE: %A\r\n}\r\n" r.Number r.Name r.IfClauses (r.ThenStatements |> operationFormat) r.ElseStatements

let variableFormat (v:Variable) =
    sprintf ""
 
