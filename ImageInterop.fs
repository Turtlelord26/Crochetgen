module Crochetgen.ImageInterop

open System
open System.IO

open Crochetgen.Errors
open Crochetgen.Pixel.Utils

open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing

let (>>=) result binder = 
    Result.bind binder result

let (>>-) result mapper =
    Result.map mapper result

let ImageSharpToNativeType (pixelArray: Rgb24[]) =
    pixelArray
    |> Seq.map (fun pixel -> makePixel pixel.R pixel.G pixel.B)

let resizeImage (targetWidth: int) (targetHeight: int) (image: Image<Rgb24>) =
    try
        let size = new Size(targetWidth, targetHeight)
        image.Mutate<Rgb24>(fun context -> context.Resize(size, KnownResamplers.NearestNeighbor, false) |> ignore)
        image |> Ok
    with
    | :? ArgumentNullException as e -> e.Message |> InteropNullPointer |> Error
    | :? ObjectDisposedException as e -> e.Message |> ObjectDisposed |> Error
    | :? ImageProcessingException as e -> e.Message |> ImageProcessingFailure |> Error

let loadImage (filename: string) =
    try
        Image.Load<Rgb24>(filename) |> Ok
    with
    | :? ArgumentNullException as e-> e.Message |> InteropNullPointer |> Error
    | :? UnknownImageFormatException -> filename |> ImageFormatNotRecognized |> Error
    | :? InvalidImageContentException -> filename |> ImageContentInvalid |> Error
    | :? NotSupportedException -> filename |> ImageFormatNotSupported |> Error
    | :? FileNotFoundException as e -> e.Message |> FileNotFound |> Error

let extractPixels (image: Image<Rgb24>) =
    let pixelArray = [|for i in 1..(image.Width * image.Height) -> new Rgb24()|]
    image.CopyPixelDataTo(pixelArray)
    pixelArray
    |> ImageSharpToNativeType

let seqIsNotEmpty image =
    match Seq.isEmpty image with
    | false -> image |> Ok
    | true -> EmptyImage |> Error

let getPixelsFromFile (filename: string) resizer =
    filename
    |> loadImage
    >>= resizer
    >>- extractPixels
    >>= seqIsNotEmpty

let loadPixelDataFromImageFile filename targetWidth targetHeight  =
    resizeImage targetWidth targetHeight
    |> getPixelsFromFile filename
    