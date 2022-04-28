module Crochetgen.Pattern

open Crochetgen.CompressedRow
open Crochetgen.CompressedRow.Utils

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
