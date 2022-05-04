namespace Crochetgen.PixelCount

open Crochetgen.Pixel

type PixelCount =
    { pixel : Pixel;
      count : int }

module Utils =

    open Crochetgen.Pixel.Utils

    let makePixelCount (pixel, count) =
        { pixel = pixel;
          count = count }

    let getPixel pixelCount = 
        pixelCount.pixel
    
    let getCount pixelCount = 
        pixelCount.count
    
    let pixelCountToString pixelCount =
        pixelToString pixelCount.pixel + ", " + string pixelCount.count