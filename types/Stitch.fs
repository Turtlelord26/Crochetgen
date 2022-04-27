namespace Crochetgen.Stitch

open Crochetgen.Pixel

type Stitch =
    | SingleStitch of Color: Pixel
    | DoubleStitch of Color: Pixel
    | TripleStitch of Color: Pixel

module Utils =

    open Crochetgen.Pixel.Utils

    let makeTripleStitch pixel = TripleStitch pixel

    let makeDoubleStitch pixel = DoubleStitch pixel

    let makeSingleStitch pixel = SingleStitch pixel

    let extractPixel stitch =
        match stitch with
        | SingleStitch pixel -> pixel
        | DoubleStitch pixel -> pixel
        | TripleStitch pixel -> pixel

    let stitchToString stitch =
        match stitch with
        | SingleStitch pixel -> $"{pixelToString pixel} ss"
        | DoubleStitch pixel -> $"{pixelToString pixel} ds"
        | TripleStitch pixel -> $"{pixelToString pixel} ts"