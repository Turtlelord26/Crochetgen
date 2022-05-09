module Crochetgen.ImageFormatter

open Crochetgen.PixelCount.Utils

let unflattenAndCompressImageRows width =

    let accumulateRowPixelCounts =
        Seq.map makePixelCountAtOne
        >> mergeAdjacentSameColorPixelCounts

    Seq.chunkBySize width
    >> Seq.map Seq.ofArray
    >> Seq.map accumulateRowPixelCounts

let decompressAndFlattenImageRows image =
    
    let decompressAndFlattenRow =
        Seq.map unwrapPixelCount
        >> Seq.concat
    
    image
    |> Seq.map decompressAndFlattenRow
    |> Seq.concat