module Crochetgen.ColorSimplification

open Crochetgen.ColorSelection
open Crochetgen.ImageFormatter
open Crochetgen.Pixel.Utils
open Crochetgen.PixelCount.Utils

let makeSimplifier numColors: seq<Pixel.Pixel> -> Pixel.Pixel -> Pixel.Pixel =

    let mostCommonColors numColors =
        Seq.countBy (fun pixel -> pixel)
        >> Seq.map ((<||) makePixelCount)
        >> selectColors numColors

    let simplify colorSet pixel =
        colorSet
        |> Seq.minBy (pixelDifference pixel)

    mostCommonColors numColors
    >> simplify

let simplifyColors numColors width flatImageData =
    
    let sharpenedPixels = 
        flatImageData
        |> Seq.map (roundPixel 8)
        |> Seq.cache
    
    let simplifier = makeSimplifier numColors sharpenedPixels

    let sharpenedPixelCounts =
        sharpenedPixels
        |> unflattenAndCompressImageRows width
    
    let simplifiedPixelCounts =
        sharpenedPixelCounts
        |> Seq.map (Seq.map (simplifier |> applyToPixel))
        |> Seq.map mergeAdjacentSameColorPixelCounts

    simplifiedPixelCounts
