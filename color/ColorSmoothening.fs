module Crochetgen.ColorSmoothening

let smoothenColors radius passes =

    let threshold = float (2 * radius - 1) ** 2 / 2.0 + 0.5 |> int

    let smoothenConditionOrDefault original pixelCount =
        match pixelCount |> snd with
        | i when i >= threshold -> pixelCount |> fst
        | _ -> original

    let headPixelOrDefault original pixelCounts =
        match pixelCounts |> Array.length with
        | 0 -> original
        | _ ->
            pixelCounts
            |> Array.head
            |> smoothenConditionOrDefault original
    
    let smoothen pixel =
        Array.countBy id
        >> Array.sortByDescending snd
        >> headPixelOrDefault pixel
    
    let smoothenSquare (image: Pixel.Pixel[,]) i1 i2 pixel =
        let square = image[i1 - radius .. i1 + radius , i2 - radius .. i2 + radius]
        
        Array.init (Array2D.length1 square) (fun row -> square[row+(Array2D.base1 square),*])
        |> Array.concat
        |> smoothen pixel
    
    let smootheningPass image =
        image
        |> Array2D.mapi (smoothenSquare image)
    
    seq {for _ in 1 .. passes -> smootheningPass}
    |> Seq.fold (>>) id
