module Crochetgen.Writer

open System
open System.IO

open Crochetgen.Errors
open Crochetgen.Errors.Print

let writeErrors errors =
    errors
    |> printErrors
    |> printfn "%s"

let writer filepath =
    try
        File.CreateText(filepath)
        |> Ok
    with
    | :? UnauthorizedAccessException -> filepath |> UnauthorizedAccess |> Error
    | :? ArgumentNullException -> PathNull |> Error
    | :? PathTooLongException -> PathTooLong |> Error
    | :? FileNotFoundException as e -> e.Message |> FileNotFound |> Error
    | :? DirectoryNotFoundException as e -> e.Message |> DirectoryNotFound |> Error
    | :? NotSupportedException -> filepath |> MalformedPath |> Error

let write (patternText: string) (writer: StreamWriter) =
    try
        writer.Write(patternText)
        writer.Close()
        Ok ()
    with
    | :? ObjectDisposedException as e -> e.Message |> ObjectDisposed |> Error
    | :? NotSupportedException -> WriteNotSupported |> Error
    | :? IOException -> patternText |> WriteIO |> Error

let writeOutput filepath outText =
    writer filepath
    |> Result.bind (write outText)
