module Crochetgen.Pattern

open Crochetgen.CountedRow
open Crochetgen.CountedRow.Utils

let collapseRowCount countedRow =

    let accumulateStitches accumulator colorCount =
        match accumulator with
        | prevColorCount :: remainder when prevColorCount.color = colorCount.color -> 
            incrementColorCount prevColorCount :: remainder
        | _ -> colorCount :: accumulator

    let newCounts = 
        countedRow.colorCounts
        |> Seq.fold accumulateStitches []
    
    { stitchType = countedRow.stitchType; colorCounts = newCounts }

let concatWithDelimiter delimiter string1 string2 =
    string1 + delimiter + string2

let stitchConcat =
    concatWithDelimiter ", "

let prefixConcat =
    concatWithDelimiter ".\t"

let rowConcat =
    concatWithDelimiter "\n"
    
let makeRowPattern =
    makeCountedRow
    >> collapseRowCount
    >> countedRowToString stitchConcat

let makePattern stitches =

    let rowLabels = Seq.initInfinite string

    let patternFrom =
        Seq.map makeRowPattern
        >> Seq.map2 prefixConcat rowLabels
        >> Seq.reduce rowConcat
        
    patternFrom stitches
