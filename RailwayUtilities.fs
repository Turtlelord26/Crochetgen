module Crochetgen.RailwayUtilities

let tee func arg =
    func arg
    arg

let errorableTee func arg =
    match func arg with
    | Some error -> Error error
    | None -> Ok arg
