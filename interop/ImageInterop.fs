module Crochetgen.ImageInterop

open System
open System.IO

open Crochetgen.Errors
open Crochetgen.Errors.Print
open Crochetgen.Pixel
open Crochetgen.Pixel.Utils

//https://docs.sixlabors.com/articles/imagesharp/index.html
//https://docs.sixlabors.com/api/ImageSharp/SixLabors.ImageSharp.PixelFormats.Rgb24.html
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing

let getPixelArray =

    let getImageSharpPixelArray (image: Image<Rgb24>) =
        let pixelArray = 
            Array.create (image.Width * image.Height) (new Rgb24())
        image.CopyPixelDataTo(pixelArray)
        pixelArray

    let toPixel (foreignPixel: Rgb24) =
        makePixel foreignPixel.R foreignPixel.G foreignPixel.B

    getImageSharpPixelArray
    >> Array.map toPixel

let loadImage (filename: string) =
    try
        Image.Load<Rgb24>(filename) |> Ok
    with
    | :? ArgumentNullException as e-> e.Message |> InteropNullPointer |> Error
    | :? UnknownImageFormatException -> filename |> ImageFormatNotRecognized |> Error
    | :? InvalidImageContentException -> filename |> ImageContentInvalid |> Error
    | :? NotSupportedException -> filename |> ImageFormatNotSupported |> Error
    | :? FileNotFoundException as e -> e.Message |> FileNotFound |> Error

let resizeImage (targetWidth: int) (targetHeight: int) (image: Image<Rgb24>) =
    try
        let size = new Size(targetWidth, targetHeight)
        image.Mutate<Rgb24>(fun context -> context.Resize(size, KnownResamplers.Spline, false) |> ignore)
        image.SaveAsPng("resizedOutputForDebug.png")
        image |> Ok
    with
    | :? ArgumentNullException as e -> e.Message |> InteropNullPointer |> Error
    | :? ObjectDisposedException as e -> e.Message |> ObjectDisposed |> Error
    | :? ImageProcessingException as e -> e.Message |> ImageProcessingFailure |> Error

let saveImage (filepath: string) (image: Image<Rgb24>) =
    try
        image.SaveAsPng(filepath)
        image |> Ok
    with
    | :? ArgumentNullException as e -> e.Message |> InteropNullPointer |> Error
    | :? DirectoryNotFoundException as e -> e.Message |> DirectoryNotFound |> Error

let savePixelsToImage filepath width height pixels =

    let pixelToIPixel pixel =
        new Rgb24(pixel.R, pixel.G, pixel.B)

    let loadPixelData (iPixels: Rgb24[]) =
        try
            Image.LoadPixelData(iPixels, width, height) |> Ok
        with
        | :? ArgumentException as e -> e.Message |> PixelDimensionsMismatch |> Error
    
    let saveImage =
        Array.map pixelToIPixel
        >> loadPixelData
        >> Result.bind (saveImage filepath)
    
    match saveImage pixels with
    | Ok _ -> ()
    | Error e -> e |> printError |> printfn "%s"