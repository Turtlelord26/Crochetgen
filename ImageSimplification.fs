module Crochetgen.ImageSimplification

open Crochetgen.Pixel.Utils

let sharpenImage image =
    image
    |> Seq.map roundPixel

let mostCommonColors numColors pixels =
    pixels
    |> Seq.countBy (fun pixel -> pixel)
    |> Seq.sortBy (fun pixcount -> match pixcount with (_, i) -> i)
    |> Seq.truncate numColors
    |> Seq.map (fun pixcount -> match pixcount with (pix, _) -> pix)

let colorDifferenceMagnitude color1 color2 =
    colorDifference color1 color2
    |> abs

let simplify colorSet pixel =
    colorSet
    |> Seq.minBy (colorDifferenceMagnitude pixel)

let makeSimplifier numColors image =
    image
    |> mostCommonColors numColors
    |> simplify

let processImage numColors image: seq<Pixel.Pixel> =
    let sharpenedImage = sharpenImage image
    let simplifier = 
        makeSimplifier numColors sharpenedImage
    sharpenedImage
    |> Seq.map simplifier