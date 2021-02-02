// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
// to open in F# Interactive
// #r """C:\Users\Tomasz\source\repos\fparsec-master\FParsec\bin\Debug\net45\FParsecCS.dll"""
// #r """C:\Users\Tomasz\source\repos\fparsec-master\FParsec\bin\Debug\net45\FParsec.dll"""

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

