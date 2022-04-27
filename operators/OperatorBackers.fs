module Crochetgen.OperatorBackers

open Crochetgen.Errors

let addOptions option1 option2 =

    let addErrors error1 error2 =
        { errors = List.append error1.errors error2.errors }

    match option1, option2 with
    | None, None -> None
    | Some e, None -> Some e
    | None, Some f -> Some f
    | Some e, Some f -> addErrors e f |> Some