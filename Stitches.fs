module Crochetgen.Stitches

open Crochetgen.Row.Utils
open Crochetgen.RowCount.Utils
open Crochetgen.Stitch

let unflattenRows rowLength =
    Seq.chunkBySize rowLength
    >> Seq.map Seq.ofArray

let mapToStitches =

    let reducer accumulator boxedRow = 
        let row = List.head boxedRow
        let head = List.head accumulator
        match compareRowCountColors head row with
        | 0 -> List.insertAt 0 (head |> incrementRowCount) (List.tail accumulator)
        | _ -> accumulator |> List.insertAt 0 row
    
    let makeStitchesMatching colors stitchList count =

        let stitchesFromTypeList stitchList colors =
            let rowOfColors = 
                makeRow colors
            stitchList
            |> Seq.map rowOfColors

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
        
    Seq.map makeRowCount
    >> Seq.map (fun row -> [row])
    >> Seq.reduce reducer
    >> Seq.map toStitches
    >> Seq.concat

let prependFoundationRow stitchRows =
    let makeFoundation = 
        Seq.head
        >> rowColors
        >> makeChainRow
    
    let foundation =
        stitchRows
        |> makeFoundation

    stitchRows
    |> Seq.insertAt 0 foundation

let pixelsToStitches rowLength =
    unflattenRows rowLength
    >> mapToStitches
    >> prependFoundationRow

let makeStitchesFromPixels rowLength pixels =
    pixels
    |> pixelsToStitches rowLength