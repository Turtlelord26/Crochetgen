namespace Crochetgen.Stitch

type Stitch =
    | Chain
    | SingleStitch
    | DoubleStitch
    | TripleStitch

module Utils =

    let stitchAbbreviation stitch =
        match stitch with
        | Chain -> "ch"
        | SingleStitch -> "ss"
        | DoubleStitch -> "ds"
        | TripleStitch -> "ts"