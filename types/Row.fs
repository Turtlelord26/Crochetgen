namespace Crochetgen.Row

open Crochetgen.Pixel
open Crochetgen.Stitch

type Row = { stitch: Stitch; colors: seq<Pixel> }

module Utils =

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
    
    let getStitch row =
        row.stitch
    
    let getColors row =
        row.colors
