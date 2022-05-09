module Crochetgen.ColorSimplification

open Crochetgen.ColorSelection
open Crochetgen.Pixel.Utils
open Crochetgen.PixelCount.Utils
open Crochetgen.Writer
open Crochetgen.ColorSelection



let simplifyColors simplifier (sharpenedPixelCounts: seq<seq<PixelCount.PixelCount>>) =
    
    let simplifiedPixelCounts =
        sharpenedPixelCounts
        |> Seq.map (Seq.map (simplifier |> applyToPixel))
        |> Seq.map mergeAdjacentSameColorPixelCounts

    simplifiedPixelCounts
