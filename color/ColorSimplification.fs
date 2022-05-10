module Crochetgen.ColorSimplification

open Crochetgen.ColorSelection
open Crochetgen.Pixel.Utils
open Crochetgen.PixelCount.Utils
open Crochetgen.Writer
open Crochetgen.ColorSelection

let makeSimplifier colorSet =   

    let simplify colorSet pixel =
        colorSet
        |> Seq.minBy (pixelDifference pixel)
    
    colorSet
    |> simplify

let simplifyColors colorSet (sharpenedPixelCounts: seq<seq<PixelCount.PixelCount>>) =

    let simplifier = makeSimplifier colorSet

    let simplifiedPixelCounts =
        sharpenedPixelCounts
        |> Seq.map (Seq.map (simplifier |> applyToPixel))
        |> Seq.map mergeAdjacentSameColorPixelCounts

    simplifiedPixelCounts
