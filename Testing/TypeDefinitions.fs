module TypeDefinitions
open System.Text

// TYPES DEFINITIONS
type Source = 
    | NotSet
    | SetByUser
    | SetByExternalProgram
    | SetByDatabaseQuery

type ProbabilityMode =
    | YesNo
    | ZeroTen
    | HundredAverage
    | HundredIndependent
    | HundredDependent
    | IncrDecr
    | Custom
    | Fuzzy

