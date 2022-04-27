module Crochetgen.Operators

open Crochetgen.OperatorBackers

let (>>=) option binder = 
    Option.bind binder option

let (++) option1 option2 =
    addOptions option1 option2

let chain option func arg =
    match option with
    | None -> func arg
    | Some errors -> Some errors
