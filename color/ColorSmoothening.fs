module Crochetgen.ColorSmoothening

open Crochetgen.PixelCount.Utils
open Crochetgen.SeqUtils

let smoothenRowCount =

    let smoothenCondition count1 count2 =
        count1 <= 1 && count2 >= 3
        || count1 <= 2 && count2 >= 7

    let smoothen pixelCount accumulator  =
        let prevPixelCount = accumulator |> Seq.head
        let nextPixelCount = pixelCount |> Seq.head
        match prevPixelCount |> getCount, nextPixelCount |> getCount with
        | i, j when smoothenCondition i j -> 
            Seq.insertAt 0 (makePixelCount (nextPixelCount |> getPixel) (i + j)) (Seq.tail accumulator)
        | i, j when smoothenCondition j i -> 
            Seq.insertAt 0 (makePixelCount (prevPixelCount |> getPixel) (i + j)) (Seq.tail accumulator)
        | _, _ -> 
            Seq.insertAt 0 nextPixelCount accumulator

    Seq.map seqify
    >> Seq.reduceBack smoothen
    >> mergeAdjacentSameColorPixelCounts

let smoothenColors imageRows =

    let rec smootheningPasses passes image =
        match passes with
        | i when i > 0 -> image |> Seq.map smoothenRowCount |> smootheningPasses (i - 1)
        | _ -> image
    
    imageRows
    |> smootheningPasses 2