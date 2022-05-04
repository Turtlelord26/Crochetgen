module Crochetgen.Pattern

open Crochetgen.CompressedRow
open Crochetgen.CompressedRow.Utils
open Crochetgen.StringUtils

let collapseRowCount compressedRow =

    let accumulateStitches accumulator colorCount =
        let newColorCount = Seq.head colorCount
        let head = Seq.head accumulator
        match head with
        | prevColorCount when prevColorCount.color = newColorCount.color -> 
            accumulator
            |> Seq.tail
            |> Seq.insertAt 0 (incrementColorCount prevColorCount) 
        | _ -> 
            accumulator
            |> Seq.insertAt 0 newColorCount

    let newCounts =
        compressedRow.colorCounts
        |> Seq.map (fun row -> Seq.ofList [row])
        |> Seq.reduce accumulateStitches
    
    { stitchType = compressedRow.stitchType; colorCounts = newCounts }
    
let makeRowPattern =
    compressRow
    >> collapseRowCount
    >> compressedRowToString concatAsList

let makePattern stitchRows =

    let rowLabels = Seq.initInfinite string

    let patternFrom =
        Seq.map makeRowPattern
        >> Seq.map2 concatAsPrefix rowLabels
        >> Seq.reduce concatAsNewline
        
    patternFrom stitchRows
