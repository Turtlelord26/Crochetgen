module Crochetgen.InputValidation

open System

open Crochetgen.Errors
open Crochetgen.Errors.Fail
open Crochetgen.Errors.OptionUtils

let validateNumOfArgs argv =
    match Array.length argv with
    | 5 -> None
    | _ -> MalformedInput |> fail

let validateNonnegativeArgs =

    let validateNonnegative integer =
        try 
            match integer |> int with
            | i when i > 0 -> None
            | _ -> integer |> NonpositiveIntegerInput |> fail
        with
        | :? FormatException -> integer |> IntegerParsingError |> fail
        | :? OverflowException -> integer |> IntegerOverflow |> fail

    Seq.map validateNonnegative
    >> Seq.reduce addOptions

let validateInput (argv: string[]) =
    match validateNumOfArgs argv with
    | Some errors -> Some errors
    | None -> validateNonnegativeArgs [argv[1]; argv[2]; argv[3]]
