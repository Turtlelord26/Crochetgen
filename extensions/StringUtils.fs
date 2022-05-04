module Crochetgen.StringUtils

let concatStrings delimiter string1 string2 =
    string1 + delimiter + string2

let concatAsNewline = concatStrings "\n"

let concatAsLabel = concatStrings ": "

let concatAsList = concatStrings ", "

let concatAsPrefix = concatStrings "\t"