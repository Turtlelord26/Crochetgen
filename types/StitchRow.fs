namespace Crochetgen.StitchRow

open Crochetgen.PixelCount
open Crochetgen.Stitch

type StitchRow = 
    { stitchType: Stitch; 
      pixelCounts: seq<PixelCount> }

module Utils =

    open Crochetgen.PixelCount.Utils
    open Crochetgen.Stitch.Utils
    open Crochetgen.StringUtils

    let makeStitchRow stitchType pixelCounts =
        { stitchType = stitchType; 
          pixelCounts = pixelCounts }

    let getStitchType compressedRow =
        compressedRow.stitchType
    
    let getPixelCounts compressedRow =
        compressedRow.pixelCounts
    
    let stitchRowToString stitchRow =
        stitchRow.pixelCounts
        |> Seq.map pixelCountToString
        |> Seq.map (fun pixString -> stitchRow.stitchType |> stitchAbbreviation |> (+) pixString)
        |> Seq.reduce concatAsList
