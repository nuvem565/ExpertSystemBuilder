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

type Derivation =
    | AllRulesUsed
    | ChosenByUser
    | NonRedundant
    //| FirstRuleOnly


type BasicAttribute =
    | Subject of string option
    | Author of string option
    | StartingText of string option
    | EndingText of string option
    | ExternalProgram of (string * string list * float list) option
    | Threshold of float // threshold value for the choice to be displayed at the end of the run. If greater then display. Optional. Zero default
    | Probability of ProbabilityMode // modes of probability confidence values system
    | Confidence of float // optional. Only for prob. system 5 - custom formula. Set initial confidence value for choice which can be other than zero in this mode
    | DisplayRules of bool // whether the rules are displayed when the expert  system is run. Deafult - no (false)
    | Derivation of Derivation //  A(ll rules used - default) | F(irst rule only) | N(on-redundant rules)
    | FuzzyThreshold of float 
    | LoopOver of string option // If true, all values in qualifiers will be reset to 0 after one run of the program. Default false. Should be true in fuzzy control.
    | CSV of char []
    | OrCurrent of bool // specify if current (in this iteration) value of fuzzy qualifier has to be calculate with new by fuzzy sum operation (fuzzy or)

type BasicInfo = 
    {
        Subject: BasicAttribute
        Author: BasicAttribute
        StartingText: BasicAttribute
        EndingText: BasicAttribute
        ExternalProgram: BasicAttribute
        DisplayThreshold: BasicAttribute
        Probability: BasicAttribute
        InitChoiceConfidence: BasicAttribute
        DisplayRules: BasicAttribute
        Derivation: BasicAttribute
        FuzzyThreshold: BasicAttribute
        LoopOverVariable: BasicAttribute
        CSVDelimiter: BasicAttribute
        OrCurrentValue: BasicAttribute
    }


