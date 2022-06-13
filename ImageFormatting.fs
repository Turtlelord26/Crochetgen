module Crochetgen.ImageFormatting

open Crochetgen.Pixel.Utils
open Crochetgen.PixelCount.Utils

let sharpenImage image = 
    image
    |> Seq.map (roundPixel 8)
    |> Seq.cache

let unflattenAndCompressImageRows width =

    let accumulateRowPixelCounts =
        Seq.map makePixelCountAtOne
        >> mergeAdjacentSameColorPixelCounts

    Seq.chunkBySize width
    >> Seq.map Seq.ofArray
    >> Seq.map accumulateRowPixelCounts

let decompressAndFlattenImageRows image =

    let unwrapPixelCount pixelCount =
        fun _ -> pixelCount |> getPixel
        |> Seq.init (pixelCount |> getCount)
    
    let decompressAndFlattenRow =
        Seq.map unwrapPixelCount
        >> Seq.concat
    
    image
    |> Seq.map decompressAndFlattenRow
    |> Seq.concat
