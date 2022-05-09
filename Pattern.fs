module Crochetgen.Pattern

open Crochetgen.StitchRow.Utils
open Crochetgen.StringUtils

let makePattern stitchRows =

    let rowLabels = Seq.initInfinite string

    let patternFrom =
        Seq.map stitchRowToString
        >> Seq.map2 concatAsPrefix rowLabels
        >> Seq.reduce concatAsNewline
        
    patternFrom stitchRows
