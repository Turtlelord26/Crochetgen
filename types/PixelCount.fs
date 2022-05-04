namespace Crochetgen.PixelCount

open Crochetgen.Pixel

type PixelCount =
    { pixel : Pixel;
      count : int }

module Utils =

    open Crochetgen.Pixel.Utils
    open Crochetgen.StringUtils

    let makePixelCount pixel count =
        { pixel = pixel;
          count = count }
    
    let makePixelCountFromTuple (pixel, count) =
        makePixelCount pixel count
    
    let makePixelCountAtOne pixel =
        makePixelCount pixel 1

    let getPixel pixelCount = 
        pixelCount.pixel
    
    let getCount pixelCount = 
        pixelCount.count

    let incrementPixelCount pixelCount =
        makePixelCount (pixelCount |> getPixel) (pixelCount |> getCount |> (+) 1)
    
    let pixelCountToString pixelCount =
        concatAsLabel (pixelToString pixelCount.pixel) (string pixelCount.count)