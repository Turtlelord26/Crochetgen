module Crochetgen.Stitches

open Crochetgen.Pixel.Utils
open Crochetgen.Stitch
open Crochetgen.Stitch.Utils



let extractPixelRow stitchRow =
    stitchRow |> Seq.map extractPixel

let comparePixelRows row1 row2 =
    Seq.compareWith (fun pix1 pix2 -> colorDifference pix1 pix2) row1 row2

let comparePixelsToStitches pixelRow stitchRow =
    stitchRow
    |> extractPixelRow
    |> comparePixelRows pixelRow

let pixelsAndStitchesOfSameColors pixelRow stitchRow =
    match comparePixelsToStitches pixelRow stitchRow with
    | 0 -> true 
    | _ -> false

let processPixelRow pixelRow stitchRow stitchRemainder =
    let colorsMatch =  
        pixelsAndStitchesOfSameColors pixelRow stitchRow
    match Seq.head stitchRow with
    | DoubleStitch _ when colorsMatch -> Seq.map makeTripleStitch pixelRow :: stitchRemainder
    | SingleStitch _ when colorsMatch -> Seq.map makeDoubleStitch pixelRow :: stitchRemainder
    | _ -> Seq.map makeSingleStitch pixelRow :: stitchRow :: stitchRemainder

let makeStitches pixelRows =

    let rec accumulateStitches pixelRows stitchesList =
        match pixelRows, stitchesList with
        | pixelRow :: pixelRemainder, stitchRow :: stitchRemainder -> 
            processPixelRow pixelRow stitchRow stitchRemainder 
            |> accumulateStitches pixelRemainder
        | pixelRow :: pixelRemainder, [] ->
            [Seq.map makeSingleStitch pixelRow] 
            |> accumulateStitches pixelRemainder
        | [], _ -> 
            stitchesList
            |> Seq.ofList

    let pixelRowList = List.ofSeq pixelRows
    accumulateStitches pixelRowList []

let getRows rowLength pixelSeq =
    pixelSeq
    |> Seq.chunkBySize rowLength
    |> Seq.map Seq.ofArray

let pixelsToStitches rowLength =
    getRows rowLength
    >> makeStitches

let makeStitchesFromPixels rowLength pixels =
    pixelsToStitches rowLength pixels