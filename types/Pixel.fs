namespace Crochetgen.Pixel

open System

type Pixel = { R: byte; G: byte; B:byte }

module Utils =

    let makePixel r g b =
        { R = r; G = g; B = b }

    let colorDifference color1 color2 =
        ((color1.R - color2.R) |> int)
        + ((color1.G - color2.G) |> int)
        + ((color1.B - color2.B) |> int)

    let pixelToString pixel =
        sprintf "%02x%02x%02x" pixel.R pixel.G pixel.B

    let roundPixel pixel =
        let roundTo5 (b: byte): byte =
            let d = double b / 5.
            Math.Round(d) * 5.
            |> byte
        { R = roundTo5 pixel.R; G = roundTo5 pixel.G; B = roundTo5 pixel.B }