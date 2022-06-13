module Crochetgen.Stitches

open Crochetgen.ImageFormatting
open Crochetgen.PixelCount.Utils
open Crochetgen.RowCount.Utils
open Crochetgen.StitchRow.Utils
open Crochetgen.Stitch
open Crochetgen.SeqUtils

let mapToStitches pixels =

    let aggregateRowCounts accumulator boxedRow = 
        let row = Seq.head boxedRow
        let head = Seq.head accumulator
        match compareRowCountColors head row with
        | 0 -> Seq.insertAt 0 (head |> incrementRowCount) (Seq.tail accumulator)
        | _ -> accumulator |> Seq.insertAt 0 row
    
    let makeStitchesMatching colors stitchList count =

        let makeStitchRowOfColors colors stitch =
            makeStitchRow stitch colors

        let stitchesFromTypeList stitchList colors =
            stitchList
            |> Seq.map (makeStitchRowOfColors colors)

        let rec stitchRecurser count stitches =
            match count with
            | 0 -> stitches
            | _ -> stitches |> Seq.append (stitchesFromTypeList stitchList colors) |> stitchRecurser (count - 1)
        
        stitchRecurser count []
    
    let toStitches rowCount =

        let makeColoredStitchesMatching = 
            rowCount
            |> rowCountColors
            |> makeStitchesMatching
        
        match rowCount |> rowCountCount with
        | i when i % 2 = 0 -> makeColoredStitchesMatching [DoubleStitch] (i / 2)
        | j when j % 3 = 0 -> makeColoredStitchesMatching [TripleStitch] (j / 3)
        | k when k % 5 = 0 -> makeColoredStitchesMatching [DoubleStitch; SingleStitch; DoubleStitch] (k / 5)
        | l when l % 7 = 0 -> makeColoredStitchesMatching [TripleStitch; SingleStitch; TripleStitch] (l / 7)
        | m when m % 11 = 0 -> makeColoredStitchesMatching [TripleStitch; SingleStitch; TripleStitch; SingleStitch; TripleStitch] (m / 7)
        | n when n % 13 = 0 -> makeColoredStitchesMatching [TripleStitch; DoubleStitch; TripleStitch; DoubleStitch; TripleStitch] (n / 7)
        | o -> makeColoredStitchesMatching [SingleStitch] o

    let toStitches =    
        Seq.map makeRowCount
        >> Seq.map seqify
        >> Seq.reduce aggregateRowCounts
        >> Seq.map toStitches
        >> Seq.concat
    
    pixels |> toStitches

let prependFoundationRow stitchRows =
    
    let makeFoundation = 
        Seq.head
        >> getPixelCounts
        >> makeStitchRow Chain

    stitchRows
    |> Seq.insertAt 0 (stitchRows |> makeFoundation)

let makeStitchesFromPixels pixels =
    pixels
    |> unpackRows
    |> Seq.map compressPixels
    |> mapToStitches
    |> prependFoundationRow