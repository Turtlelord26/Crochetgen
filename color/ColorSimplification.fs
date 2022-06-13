module Crochetgen.ColorSimplification

open Crochetgen.Pixel.Utils

let simplifyColors colorSet =

    let simplify pixel =
        colorSet
        |> Seq.minBy (pixelDifference pixel)

    Array2D.map simplify
