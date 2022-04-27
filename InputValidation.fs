module Crochetgen.InputValidation

open System

open Crochetgen.Errors
open Crochetgen.Errors.Fail
open Crochetgen.Operators

let validateNonNegative integer =
    try 
        match integer |> int with
        | i when i > 0 -> None
        | _ -> integer |> NonpositiveIntegerInput |> fail
    with
    | :? FormatException -> integer |> IntegerParsingError |> fail
    | :? OverflowException -> integer |> IntegerOverflow |> fail

let validateIntArgs numColors targetWidth targetHeight =
    validateNonNegative targetHeight
    ++ validateNonNegative targetWidth
    ++ validateNonNegative numColors

let validateNumOfArgs argv =
    match Array.length argv with
    | 5 -> None
    | _ -> MalformedInput |> fail

let validateInput (argv: string[]) =
    let chainableValidateIntArgs = validateIntArgs argv[1] argv[2]
    chain (validateNumOfArgs argv) chainableValidateIntArgs argv[3]
