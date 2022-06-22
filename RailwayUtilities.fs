module Crochetgen.RailwayUtilities

let tee func arg =
    func arg
    arg

let errorableTee func arg =
    match func arg with
    | Ok () -> Ok arg
    | Error error -> Error error
