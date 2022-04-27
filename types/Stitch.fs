namespace Crochetgen.Stitch

type Stitch =
    | SingleStitch
    | DoubleStitch
    | TripleStitch

module Utils =

    let stitchAbbreviation stitch =
        match stitch with
        | SingleStitch -> "ss"
        | DoubleStitch -> "ds"
        | TripleStitch -> "ts"