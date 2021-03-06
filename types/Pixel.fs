namespace Crochetgen.Pixel

open System

type Pixel = 
    { R: byte; 
      G: byte; 
      B: byte }

module Utils =

    let makePixel r g b =
        { R = r; 
          G = g; 
          B = b }

    let pixelDifference color1 color2 =
        (color1.R - color2.R |> int |> abs)
        + (color1.G - color2.G |> int |> abs)
        + (color1.B - color2.B |> int |> abs)

    let pixelToString pixel =
        sprintf "%02x%02x%02x" pixel.R pixel.G pixel.B

    let roundPixel granularity pixel =

        let roundToGranularity granularity b =
            let d = double b / granularity
            Math.Round(d) * granularity
            |> min 255
            |> byte

        let round = roundToGranularity granularity

        makePixel (round pixel.R) 
                  (round pixel.G) 
                  (round pixel.B)