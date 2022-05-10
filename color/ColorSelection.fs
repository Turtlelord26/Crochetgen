module Crochetgen.ColorSelection

open Crochetgen.Pixel.Utils
open Crochetgen.PixelCount.Utils
open Crochetgen.SeqUtils
open Crochetgen.StringUtils
open Crochetgen.Writer

let writeColorSelection selections =

    let pixelCountsToString header =
        Seq.map pixelToString
        >> Seq.reduce concatAsNewline
        >> concatAsNewline header

    pixelCountsToString "Selected colors:" selections

let differenceFromSelectedColors selectedColors pixcount =

    let weightedDifferenceToSelectedColors candidatePixelCount selectedPixel = 
        pixelDifference (candidatePixelCount |> getPixel) selectedPixel
        * (candidatePixelCount |> getCount)

    let aggregatePixcountDifference pixelCount =
        Seq.map getPixel
        >> Seq.map (weightedDifferenceToSelectedColors pixelCount)
        >> Seq.reduce (*)

    aggregatePixcountDifference pixcount selectedColors

let selectColors numColors colorFrequencies =

    let rec selectNextColor selectedColors colorFrequencies count =
        match count with
        | 0 -> selectedColors
        | selectionsLeft -> 
            let nextSelection =
                colorFrequencies
                |> Seq.maxBy (differenceFromSelectedColors selectedColors)
            let selection =
                selectedColors
                |> Seq.insertAt 0 nextSelection
            selectNextColor selection colorFrequencies (selectionsLeft - 1)
    
    let firstColor = 
        colorFrequencies
        |> Seq.maxBy getCount

    selectNextColor [firstColor] colorFrequencies (numColors - 1)
    |> Seq.map getPixel
    
let makeColorSet numColors =
    Seq.countBy id
    >> Seq.map ((<||) makePixelCount)
    >> selectColors numColors
