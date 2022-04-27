namespace Crochetgen.Row

open Crochetgen.Pixel
open Crochetgen.Stitch

type Row = { stitch: Stitch; colors: seq<Pixel> }

module Utils =

    open Crochetgen.Pixel.Utils

    let makeSingleStitchRow pixels = 
        { stitch = SingleStitch; colors = pixels }

    let makeDoubleStitchRow pixels = 
        { stitch = SingleStitch; colors = pixels }

    let makeTripleStitchRow pixels = 
        { stitch = SingleStitch; colors = pixels }

    let compareRowColors row1 row2 = 
        Seq.compareWith colorDifference row1 row2
