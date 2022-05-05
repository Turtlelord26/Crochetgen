module Crochetgen.ImageSimplification

open Crochetgen.StringUtils
open Crochetgen.ImageIO
open Crochetgen.Pixel.Utils
open Crochetgen.PixelCount.Utils
open Crochetgen.Writer

let makeSimplifier numColors =

    let pixcountListToString header =
        Seq.sortBy getCount
        >> Seq.map pixelCountToString
        >> Seq.reduce concatAsNewline
        >> concatAsNewline header
    
    let writeColorSelection palette selections =
        let colorData =
            pixcountListToString "Detected colors:" palette
            |> concatAsList ""
            |> concatAsList (pixcountListToString "Selected colors:" selections)
        match writeColors colorData with
        | None -> ()
        | Some errors -> errors |> writeErrors
    
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
        
        let firstColor = 
            colorFrequencies
            |> Seq.maxBy getCount

        let selectedColors = 
            selectNextColor [firstColor] colorFrequencies (numColors - 1)

        writeColorSelection colorFrequencies selectedColors

        selectedColors

    let mostCommonColors numColors =
        Seq.countBy (fun pixel -> pixel)
        >> Seq.map makePixelCountFromTuple
        >> selectColors numColors
        >> Seq.map getPixel

    let simplify colorSet pixel =
        colorSet
        |> Seq.minBy (colorDifference (+) pixel)

    mostCommonColors numColors
    >> simplify

open Crochetgen.CompressedRow.Utils


let unpackAndSaveRows width height =
    
    let unpackAndSaveRow =
        getPixelCounts
        >> Seq.map (fun crow -> [for i in 1 .. (crow |> getCount) do (crow |> getPixel)])
        >> Seq.concat
    
    Seq.map unpackAndSaveRow
    >> Seq.concat
    >> savePixels "resharpenedOutputForDebug.png" width height

let mapDeadEnd func arg =
    func arg
    arg

open Crochetgen.Pattern
let makeRowPattern =
    compressRow
    >> collapseRowCount
    >> smoothenRowCount
//Interesting. I don't actually want the stitch information here, only the compressed color counts.
//Maybe I need to rearrange my data structures. Again.
//Wouldn't it have been nice to architect this in the first place.
//Compressed color counts certainly qualifies as image simplification.
//I will end up not needing rows at all, won't I.

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
    
    //I need to collapse the rows here in order to print out the resharpened image. Collapsing rows asap probably good for performance, see stitches.fs
    //Also would be good to get the color list into a user-named file --- or maybe just the selections into the first line of the pattern file.

    simplifiedImage
