namespace Crochetgen.Row

open Crochetgen.Pixel
open Crochetgen.Stitch

type Row = { stitch: Stitch; colors: seq<Pixel> }

module Utils =

    open Crochetgen.Pixel.Utils

    let makeRow pixels stitchType =
        { stitch = stitchType; colors = pixels }

    let makeChainRow pixels = 
        makeRow pixels Chain 

    let makeSingleStitchRow pixels = 
        makeRow pixels SingleStitch

    let makeDoubleStitchRow pixels = 
        makeRow pixels DoubleStitch

    let makeTripleStitchRow pixels = 
        makeRow pixels TripleStitch

    let compareRowColors row1 row2 = 
        Seq.compareWith colorDifference row1.colors row2.colors
    
    let rowColors row =
        row.colors
