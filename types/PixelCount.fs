namespace Crochetgen.PixelCount

open Crochetgen.Pixel

type PixelCount =
    { pixel : Pixel;
      count : int }

module Utils =

    open Crochetgen.Pixel.Utils
    open Crochetgen.SeqUtils
    open Crochetgen.StringUtils

    let makePixelCount pixel count =
        { pixel = pixel;
          count = count }

    let getPixel pixelCount = 
        pixelCount.pixel
    
    let getCount pixelCount = 
        pixelCount.count
    
    let applyToPixel func pixelCount =
        makePixelCount (pixelCount.pixel |> func) pixelCount.count
    
    let comparePixelCounts pixelCount1 pixelCount2 =
        match pixelDifference pixelCount1.pixel pixelCount2.pixel with
        | 0 -> pixelCount1.count - pixelCount2.count
        | i -> i
    
    let pixelCountToString pixelCount =
        concatAsLabel (pixelToString pixelCount.pixel) (string pixelCount.count)
    
    let compressPixels pixels =

        let makePixelCountAtOne pixel =
            makePixelCount pixel 1
        
        let mergeAdjacentSameColorPixelCounts pixelCounts =

            let accumulatePixelCounts pixelCount accumulator =
                let nextPixelCount = Seq.head pixelCount
                let prevPixelCount = Seq.head accumulator
                match pixelDifference nextPixelCount.pixel prevPixelCount.pixel with
                | 0 -> 
                    accumulator
                    |> Seq.tail
                    |> Seq.insertAt 0 (makePixelCount prevPixelCount.pixel (prevPixelCount.count + nextPixelCount.count)) 
                | _ -> 
                    accumulator
                    |> Seq.insertAt 0 nextPixelCount

            pixelCounts
            |> Seq.map seqify
            |> Seq.reduceBack accumulatePixelCounts
        
        pixels
        |> Seq.map makePixelCountAtOne
        |> mergeAdjacentSameColorPixelCounts
