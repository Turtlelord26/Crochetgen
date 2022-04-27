module Crochetgen.Pattern

open Crochetgen.Stitch
open Crochetgen.Stitch.Utils

type StitchCount = { stitch: Stitch; count: int }

let incrementStitchCount sc1 =
    { stitch = sc1.stitch; count = sc1.count + 1 }

let newStitchCount stitch =
    { stitch = stitch; count = 1 }

let accumulateStitches accumulator stitchCount =
    match accumulator with
    | prevStitchCount :: remainder when prevStitchCount.stitch = stitchCount.stitch -> 
        incrementStitchCount prevStitchCount :: remainder
    | _ -> stitchCount :: accumulator

let concatWithDelimiter delimiter string1 string2 =
    string1 + delimiter + string2

let stitchConcat =
    concatWithDelimiter ", "

let rowConcat =
    concatWithDelimiter "\n"

let numConcat =
    concatWithDelimiter ".\t"

let stitchCountToString stitchCount =
    $"{stitchCount.count}x " + stitchToString stitchCount.stitch

let makeRowPattern stitchRow =
    stitchRow
    |> Seq.map newStitchCount
    |> Seq.fold accumulateStitches []
    |> Seq.map stitchCountToString
    |> Seq.reduce stitchConcat

let prefixRowNumbers rows =
    let rowNumbers = Seq.initInfinite (fun i -> string i)
    Seq.map2 (fun rowNum row -> numConcat rowNum row) rowNumbers rows

let makePattern stitches =
    stitches
    |> Seq.map makeRowPattern
    |> prefixRowNumbers
    |> Seq.reduce rowConcat
