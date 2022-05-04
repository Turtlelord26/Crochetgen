namespace Crochetgen.CompressedRow

open Crochetgen.PixelCount
open Crochetgen.Stitch

type CompressedRow = { stitchType: Stitch; pixelCounts: seq<PixelCount> }

module Utils =

    open Crochetgen.PixelCount.Utils
    open Crochetgen.Row.Utils

    let getStitchType compressedRow =
        compressedRow.stitchType
    
    let getPixelCounts compressedRow =
        compressedRow.pixelCounts

    let makeCompressedRow stitchType pixelCounts =
        { stitchType = stitchType; pixelCounts = pixelCounts }

    let compressRow row =
        makeCompressedRow (row |> getStitch) 
                          (Seq.map makePixelCountAtOne (row |> getColors))
    
    let compressedRowToString =
        getPixelCounts
        >> Seq.map pixelCountToString
