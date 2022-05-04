module Crochetgen.Pattern

open Crochetgen.CompressedRow
open Crochetgen.CompressedRow.Utils
open Crochetgen.PixelCount.Utils
open Crochetgen.SeqUtils
open Crochetgen.StringUtils

let collapseRowCount compressedRow =

    let accumulateStitches accumulator colorCount =
        let newColorCount = Seq.head colorCount
        let head = Seq.head accumulator
        match head with
        | prevColorCount when (prevColorCount |> getPixel) = (newColorCount |> getPixel) -> 
            accumulator
            |> Seq.tail
            |> Seq.insertAt 0 (incrementPixelCount prevColorCount) 
        | _ -> 
            accumulator
            |> Seq.insertAt 0 newColorCount
        
    let collapse =
        getPixelCounts
        >> Seq.map seqify
        >> Seq.reduce accumulateStitches
        >> makeCompressedRow compressedRow.stitchType

    collapse compressedRow

let smoothenRowCount compressedRow =

    let combiner cc1 cc2 =
        let prevPixelCount = cc1 |> Seq.head
        let nextPixelCount = cc2 |> Seq.head
        match prevPixelCount |> getCount, nextPixelCount |> getCount with
        | i, j when i = 1 && j >= 3 -> makePixelCount (nextPixelCount |> getPixel)  (i + j) |> seqify
        | i, j when i >= 3 && j = 1 -> makePixelCount (prevPixelCount |> getPixel)  (i + j) |> seqify
        | _, _ -> Seq.append cc2 cc1

    let newCounts =
        compressedRow
        |> getPixelCounts
        |> Seq.map seqify
        |> Seq.reduce combiner
    
    makeCompressedRow compressedRow.stitchType newCounts
    
let makeRowPattern =
    compressRow
    >> collapseRowCount
    >> smoothenRowCount
    >> compressedRowToString
    >> Seq.reduce concatAsList

let makePattern stitchRows =

    let rowLabels = Seq.initInfinite string

    let patternFrom =
        Seq.map makeRowPattern
        >> Seq.map2 concatAsPrefix rowLabels
        >> Seq.reduce concatAsNewline
        
    patternFrom stitchRows
