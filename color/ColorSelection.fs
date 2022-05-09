module Crochetgen.ColorSelection

open Crochetgen.Pixel.Utils
open Crochetgen.PixelCount.Utils
open Crochetgen.SeqUtils
open Crochetgen.StringUtils

let writeColorSelection selection =
    selection
    |> Seq.map pixelToString
    |> Seq.reduce concatAsNewline
    |> concatAsNewline "Included colors:"

let differenceFromSelectedColors selectedColors pixcount =

    let weightedDifferenceToSelectedColor candidatePixelCount selectedPixel = 
        pixelDifference (candidatePixelCount |> getPixel) selectedPixel
        * (candidatePixelCount |> getCount)

    let differenceFromSelections pixelCount =
        Seq.map getPixel
        >> Seq.map (weightedDifferenceToSelectedColor pixelCount)
        >> Seq.reduce (*)

    differenceFromSelections pixcount selectedColors

let selectColors numColors image =

    let countColors =
        Seq.countBy id
        >> Seq.map ((<||) makePixelCount)
        >> Seq.cache
    
    let colorFrequencies =
        countColors image

    let rec selectNextColors count selectedColors =
        match count with
        | 0 -> selectedColors
        | selectionsLeft -> 
            let nextSelection =
                colorFrequencies
                |> Seq.maxBy (differenceFromSelectedColors selectedColors)
            selectedColors
            |> Seq.insertAt 0 nextSelection
            |> selectNextColors (selectionsLeft - 1)
    
    colorFrequencies
    |> Seq.maxBy getCount
    |> seqify
    |> selectNextColors (numColors - 1)
    |> Seq.map getPixel