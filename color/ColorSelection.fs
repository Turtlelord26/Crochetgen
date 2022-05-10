module Crochetgen.ColorSelection

open Crochetgen.Pixel.Utils
open Crochetgen.PixelCount.Utils
open Crochetgen.SeqUtils
open Crochetgen.StringUtils

let printFormatColorSelection selections =

    let pixelCountsToString header =
        Seq.map pixelToString
        >> Seq.reduce concatAsNewline
        >> concatAsNewline header

    pixelCountsToString "Selected colors:" selections
    
let selectColors numColors =

    let differenceFromSelectedColors selectedColors pixcount =

        let weightedDifferenceToSelectedColors candidatePixelCount selectedPixel = 
            pixelDifference (candidatePixelCount |> getPixel) selectedPixel
            * (candidatePixelCount |> getCount)

        let aggregatePixcountDifference pixelCount =
            Seq.map getPixel
            >> Seq.map (weightedDifferenceToSelectedColors pixelCount)
            >> Seq.reduce (*)

        aggregatePixcountDifference pixcount selectedColors
    
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

    let makeColorSet numColors colorFrequencies =
        
        let firstColor = 
            colorFrequencies
            |> Seq.maxBy getCount

        selectNextColor (firstColor |> seqify) colorFrequencies (numColors - 1)
        |> Seq.map getPixel

    Seq.countBy id
    >> Seq.map ((<||) makePixelCount)
    >> makeColorSet numColors
    >> Seq.cache
