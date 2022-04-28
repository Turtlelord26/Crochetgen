namespace Crochetgen.CompressedRow

open Crochetgen.Pixel
open Crochetgen.Stitch

type ColorCount = { color: Pixel; count: int }

type CompressedRow = { stitchType: Stitch; colorCounts: seq<ColorCount> }

module Utils =

    open Crochetgen.Pixel.Utils
    open Crochetgen.Row
    open Crochetgen.Stitch.Utils

    let makeCountedRow row =
        let makeColorCount pixel = { color = pixel; count = 1 }
        let colors = Seq.map makeColorCount row.colors
        { stitchType = row.stitch; colorCounts = colors }
    
    let incrementColorCount sc1 =
        { color = sc1.color; count = sc1.count + 1 }
    
    let colorCountToString stitchType colorCount =
        $"{pixelToString colorCount.color}: {colorCount.count} {stitchAbbreviation stitchType}";
    
    let countedRowToString concatenater =
        
        let colorCountsToStrings row =
            let stringify = colorCountToString row.stitchType
            row.colorCounts
            |> Seq.map stringify
        
        colorCountsToStrings
        >> Seq.reduce concatenater
        
