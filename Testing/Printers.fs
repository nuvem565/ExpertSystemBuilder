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
 
// Auxiliary function for saving sequence of Section type to the file
let printAll (file:StreamWriter) sections = 
    for elem in sections do
        match elem with 
        | Basic x -> 
            file.WriteLine("BASIC INFO:")
            file.WriteLine(sprintf "%A" x)
        | Qualifiers x -> 
            file.WriteLine("QUALIFIERS:")
            ResizeArray.iter (fun a -> a |> sprintf "%A" |> file.WriteLine) x
        | Choices x -> 
            file.WriteLine("CHOICES:")
            ResizeArray.iter (fun a -> a |> sprintf "%A" |> file.WriteLine) x
        | Variables x -> 
            file.WriteLine("VARIABLES:")
            ResizeArray.iter (fun a -> a |> sprintf "%A" |> file.WriteLine) x
        | Rules x -> 
            file.WriteLine("RULES:")
            ResizeArray.iter (fun a -> a |> ruleFormat |> file.Write) x


// Output to the file
let writeResult (p:Parser<Section[],unit>) str =
    match run p str with 
    | Success(result,_,_) -> 
        printfn "Please, enter the parsed objects file name:"
        let fileName = System.Console.ReadLine()
        // Default "Demo_parsed.txt path, for no user input
        let mutable filePath:string = __SOURCE_DIRECTORY__ + "\\Demo_parsed.txt"
        // In case of user input
        if fileName <> "" then
            filePath <- __SOURCE_DIRECTORY__ + "\\" + fileName + ".txt"
            
        let file = new System.IO.StreamWriter(filePath)

        // Passing StreamWriter object to save the results in apropriate format
        printAll file result
        // Closing in order to print it out to the cmd
        file.Close()        
        printfn "Success: \n" 
        printfn "%s" (File.ReadAllText(filePath))
        printfn "File creation succeded. Output directory: %s" filePath
    | Failure(errorMessage,_,_) -> printfn "Failure: %s" errorMessage
         