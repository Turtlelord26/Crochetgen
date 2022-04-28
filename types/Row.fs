namespace Crochetgen.Row

open Crochetgen.Pixel
open Crochetgen.Stitch

type Row = { stitch: Stitch; colors: seq<Pixel> }

module Utils =

    open Crochetgen.Pixel.Utils

    let makeChainRow pixels = 
        { stitch = Chain; colors = pixels }

    let makeSingleStitchRow pixels = 
        { stitch = SingleStitch; colors = pixels }

    let makeDoubleStitchRow pixels = 
        { stitch = DoubleStitch; colors = pixels }

    let makeTripleStitchRow pixels = 
        { stitch = TripleStitch; colors = pixels }

    let compareRowColors row1 row2 = 
        Seq.compareWith colorDifference row1 row2
