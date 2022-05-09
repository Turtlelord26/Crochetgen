module Crochetgen.ColorSelection

open Crochetgen.Pixel.Utils
open Crochetgen.PixelCount.Utils
open Crochetgen.StringUtils
open Crochetgen.Writer

let writeColorSelection palette selections =

    let pixelCountsToString header =
        Seq.sortByDescending getCount
        >> Seq.map pixelCountToString
        >> Seq.reduce concatAsNewline
        >> concatAsNewline header

    let colorData =
        pixelCountsToString "Detected colors:" palette
        |> concatAsNewline ""
        |> concatAsNewline (pixelCountsToString "Selected colors:" selections)

    match writeColors colorData with
    | None -> ()
    | Some errors -> errors |> writeErrors

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

    let selectedColors = 
        selectNextColor [firstColor] colorFrequencies (numColors - 1)

    writeColorSelection colorFrequencies selectedColors

    selectedColors
    |> Seq.map getPixel