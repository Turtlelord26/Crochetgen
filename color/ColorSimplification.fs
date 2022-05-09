module Crochetgen.ColorSimplification

open Crochetgen.Pixel.Utils
open Crochetgen.PixelCount.Utils
open Crochetgen.Writer
open Crochetgen.ColorSelection

let simplifyColors selectedColors: seq<seq<PixelCount.PixelCount>> -> seq<seq<PixelCount.PixelCount>> =

    writeOutput ("simplifier" + "_colors.txt") (selectedColors |> writeColorSelection) |> ignore

    let simplify pixel =
        selectedColors
        |> Seq.minBy (pixelDifference pixel)

    let simplifyRow =
        (Seq.map (simplify |> applyToPixel))
        >> mergeAdjacentSameColorPixelCounts
    
    Seq.map simplifyRow
