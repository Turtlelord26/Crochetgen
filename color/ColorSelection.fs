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

        let weightedDifferenceToSelectedColor candidatePixelCount selectedPixel = 
            pixelDifference (candidatePixelCount |> getPixel) selectedPixel
            * (candidatePixelCount |> getCount)

        let differenceFromSelections pixelCount =
            Seq.map getPixel
            >> Seq.map (weightedDifferenceToSelectedColor pixelCount)
            >> Seq.reduce (*)

        differenceFromSelections pixcount selectedColors
    
    let rec selectNextColor count colorFrequencies selectedColors =

        match count with
        | 0 -> selectedColors
        | selectionsLeft -> 
            let nextSelection =
                colorFrequencies
                |> Seq.maxBy (differenceFromSelectedColors selectedColors)

            selectedColors
            |> Seq.insertAt 0 nextSelection
            |> selectNextColor (selectionsLeft - 1) colorFrequencies

    let makeColorSet numColors colorFrequencies =
        
        let firstColor = 
            colorFrequencies
            |> Seq.maxBy getCount

        firstColor
        |> seqify
        |> selectNextColor (numColors - 1) colorFrequencies 
        |> Seq.map getPixel

    Seq.countBy id
    >> Seq.map ((<||) makePixelCount)
    >> makeColorSet numColors
    >> Seq.cache
