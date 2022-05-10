module Crochetgen.ColorSimplification

open Crochetgen.Pixel.Utils
open Crochetgen.PixelCount.Utils

let simplifyColors colorSet =

    let simplify pixel =
        selectedColors
        |> Seq.minBy (pixelDifference pixel)
    
    let simplifyRow =
        Seq.map (simplify colorSet |> applyToPixel)
        >> mergeAdjacentSameColorPixelCounts

    Seq.map simplifyRow
