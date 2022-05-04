module Crochetgen.ImageSimplification

open Crochetgen.StringUtils
open Crochetgen.ImageIO
open Crochetgen.Pixel.Utils
open Crochetgen.PixelCount.Utils

let makeSimplifier numColors =
    
    let aggregatePixcountDifference pixel count =
        Seq.map getPixel
        >> Seq.map (colorDifference (+) pixel)
        >> Seq.map ((*) count)
        >> Seq.reduce (*)

    let compareToSelectedColors selectedColors pixcount =
        aggregatePixcountDifference (getPixel pixcount) (getCount pixcount) selectedColors

    let selectColors numColors colorFrequencies =

        let rec selectNextColor selectedColors colorFrequencies count =
            match count with
            | 0 -> selectedColors
            | selectionsLeft -> 
                let nextSelection =
                    colorFrequencies
                    |> Seq.maxBy (compareToSelectedColors selectedColors)
                let selection =
                    selectedColors
                    |> Seq.insertAt 0 nextSelection
                selectNextColor selection colorFrequencies (selectionsLeft - 1)
        
        let printDebugPixcountList = //This is useful the two list printouts could reasonably write to a file.
            Seq.sortBy getCount
            >> Seq.map pixelCountToString
            >> Seq.reduce concatAsNewline
            >> printfn "%s"

        printfn "Detected colors:"
        printDebugPixcountList colorFrequencies
        
        let firstColor = 
            colorFrequencies
            |> Seq.maxBy getCount
        let selectedColors = selectNextColor [firstColor] colorFrequencies (numColors - 1)

        printfn "\nSelected colors:"
        printDebugPixcountList selectedColors

        selectedColors

    let mostCommonColors numColors =
        Seq.countBy (fun pixel -> pixel)
        >> Seq.map makePixelCount
        >> selectColors numColors
        >> Seq.map getPixel

    let simplify colorSet pixel =
        colorSet
        |> Seq.minBy (colorDifference (+) pixel)

    mostCommonColors numColors
    >> simplify

let processImage numColors width height image: seq<Pixel.Pixel> =
    
    let sharpenedImage = 
        image
        |> Seq.map (roundPixel 8)
    
    let simplifier = 
        sharpenedImage
        |> makeSimplifier numColors
    
    let simplifiedImage =
        sharpenedImage
        |> Seq.map simplifier

    sharpenedImage
    |> savePixels "sharpenedOutputForDebug.png" width height
    
    simplifiedImage
    |> savePixels "simplifiedOutputForDebug.png" width height

    simplifiedImage
