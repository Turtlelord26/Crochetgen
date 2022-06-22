module Crochetgen.ImageInterop

open System
open System.IO

open Crochetgen.Errors
open Crochetgen.Errors.Print
open Crochetgen.Pixel
open Crochetgen.Pixel.Utils
open Crochetgen.RailwayUtilities

//https://docs.sixlabors.com/articles/imagesharp/index.html
//https://docs.sixlabors.com/api/ImageSharp/SixLabors.ImageSharp.PixelFormats.Rgb24.html
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Formats
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
        let format = Image.DetectFormat(filename)
        let image = Image.Load<Rgb24>(filename)
        (image, format) |> Ok
    with
    | :? ArgumentNullException as e-> e.Message |> InteropNullPointer |> Error
    | :? UnknownImageFormatException -> filename |> ImageFormatNotRecognized |> Error
    | :? InvalidImageContentException -> filename |> ImageContentInvalid |> Error
    | :? NotSupportedException -> filename |> ImageFormatNotSupported |> Error
    | :? FileNotFoundException as e -> e.Message |> FileNotFound |> Error

let saveImage (filepath: string) (format: IImageFormat) (image: Image) =
    try
        let stream = new FileStream(filepath, FileMode.Create)
        image.Save(stream, format)
        Ok ()
    with
    | :? ArgumentNullException as e -> e.Message |> InteropNullPointer |> Error
    | :? DirectoryNotFoundException as e -> e.Message |> DirectoryNotFound |> Error

let mutate mutator (image: Image) =
    try
        image.Mutate(mutator)
        Ok ()
    with
    | :? ArgumentNullException as e -> e.Message |> InteropNullPointer |> Error
    | :? ObjectDisposedException as e -> e.Message |> ObjectDisposed |> Error
    | :? ImageProcessingException as e -> e.Message |> ImageProcessingFailure |> Error

let resizeImage (targetWidth: int) (targetHeight: int) (image: Image<Rgb24>) =
    let size = new Size(targetWidth, targetHeight)
    let resizer = fun (context: IImageProcessingContext) -> context.Resize(size, KnownResamplers.Spline, false) |> ignore
    errorableTee (mutate resizer) image

let savePixelsToImage filepath format width height pixels =

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
        >> Result.bind (errorableTee (saveImage filepath format))
    
    match saveImage pixels with
    | Ok _ -> ()
    | Error e -> e |> printError |> printfn "%s"

let appendExtension name (format: IImageFormat) =
    name + "." + Seq.head format.FileExtensions
