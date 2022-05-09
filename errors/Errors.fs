namespace Crochetgen.Errors

type Error =
    | MalformedInput
    | ImageFilenameNull
    | ImageFormatNotRecognized of filename: string
    | ImageContentInvalid of filename: string
    | ImageFormatNotSupported of filename: string
    | IntegerInputNull
    | IntegerParsingError of input: string
    | IntegerOverflow of input: string
    | NonpositiveIntegerInput of input: string
    | UnauthorizedAccess of filepath: string
    | PathNull
    | PathTooLong
    | PixelDimensionsMismatch of message: string
    | DirectoryNotFound of filepath: string
    | MalformedPath of filepath: string
    | ObjectDisposed of message: string
    | WriteNotSupported
    | WriteIO of outText: string
    | ImageProcessingFailure of message: string
    | InteropNullPointer of message: string
    | FileNotFound of message: string
    | EmptyImage

type ErrorList = { errors: Error list }

module Fail =

    let fail error = 
        { errors=[error] } |> Some
    
module OptionUtils =

    let addOptions option1 option2 =

        let addErrors error1 error2 =
            { errors = List.append error1.errors error2.errors }

        match option1, option2 with
        | None, None -> None
        | Some e, None -> Some e
        | None, Some f -> Some f
        | Some e, Some f -> addErrors e f |> Some

module Print =

    let printError error =
        match error with
        | MalformedInput -> "Usage: Crochetgen imageFilePath numberOfColors widthInStitches heightInStitches outputFileName"
        | ImageFilenameNull -> "Image filename null."
        | ImageFormatNotRecognized filename -> $"Image format of file {filename} not recognized."
        | ImageContentInvalid filename -> $"Image content of file {filename} invalid."
        | ImageFormatNotSupported filename -> $"Image format of file {filename} not supported."
        | IntegerInputNull -> $"An integer input was null."
        | IntegerParsingError number -> $"{number} is not a parseable integer."
        | IntegerOverflow number -> $"{number} overflows."
        | NonpositiveIntegerInput input -> $"Input {input} is nonpositive. Integer inputs must be positive."
        | UnauthorizedAccess filepath -> $"User is not authorized to write to {filepath}, or it is readonly or hidden."
        | PathNull -> "Path was null."
        | PathTooLong -> "The specified path, file name, or both exceed the system-defined maximum length."
        | PixelDimensionsMismatch message -> "Length of pixel array did not match given width * height while saving an image\n" + message
        | DirectoryNotFound filepath -> $"The specified path is invalid (for example, it is on an unmapped drive).\n{filepath}"
        | MalformedPath filepath -> $"{filepath} is in an invalid format."
        | ObjectDisposed message -> message
        | WriteNotSupported -> "AutoFlush is true or the StreamWriter buffer is full, and the contents of the buffer cannot be written to the underlying fixed size stream because the StreamWriter is at the end the stream."
        | WriteIO outText -> $"An I/O error occurred while writing the output below.\n{outText}"
        | ImageProcessingFailure message -> message
        | InteropNullPointer message -> message
        | FileNotFound message -> message
        | EmptyImage -> "No information loaded from image file, presumed empty."

    let private concatWithDelimiter delimiter string1 string2 =
        string1 + delimiter + string2

    let private concatWithNewline = 
        concatWithDelimiter "\n"

    let printErrors errors =
        errors.errors
        |> List.map printError
        |> List.fold concatWithNewline ""