module Crochetgen.Stitches

open Crochetgen.Row
open Crochetgen.Row.Utils
open Crochetgen.Stitch

let makeStitches pixelRows =

    let mergeOrAppendRow pixelRow stitchRow stitchRemainder =
        let colorsMatch =  
            compareRowColors pixelRow stitchRow.colors = 0
        match stitchRow.stitch with
        | DoubleStitch when colorsMatch -> makeTripleStitchRow pixelRow :: stitchRemainder
        | SingleStitch when colorsMatch -> makeDoubleStitchRow pixelRow :: stitchRemainder
        | _ -> makeSingleStitchRow pixelRow :: stitchRow :: stitchRemainder

    let rec accumulateStitches pixelRows stitchesList =
        match pixelRows, stitchesList with
        | pixelRow :: pixelRemainder, stitchRow :: stitchRemainder -> 
            mergeOrAppendRow pixelRow stitchRow stitchRemainder 
            |> accumulateStitches pixelRemainder
        | pixelRow :: pixelRemainder, [] ->
            [makeSingleStitchRow pixelRow] 
            |> accumulateStitches pixelRemainder
        | [], _ -> 
            stitchesList
            |> Seq.ofList

    let pixelRowList = List.ofSeq pixelRows
    accumulateStitches pixelRowList []

let prependFoundationRow stitchRows =
    let foundation = (Seq.head stitchRows).colors |> makeChainRow

    stitchRows
    |> Seq.insertAt 0 foundation

let getRows rowLength pixelSeq =
    pixelSeq
    |> Seq.chunkBySize rowLength
    |> Seq.map Seq.ofArray

let pixelsToStitches rowLength =
    getRows rowLength
    >> makeStitches
    >> prependFoundationRow

let makeStitchesFromPixels rowLength pixels =
    pixelsToStitches rowLength pixels