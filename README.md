### Crochetgen 

Crochetgen is a .NET console application that converts an image into a simple crochet stitch pattern.

Crochetgen accepts an image file as input, of any of the standard image filetypes, and will output a .txt containing the pattern and a .png with an image preview of the pattern. The preview is sized such that one pixel = one single stitch.

Currently generates patterns of single, double, and triple stitches.

When run, will print Complete to the console on success, or error messages.

Usage:
`dotnet {.../Crochetgen.dll} {path to input image} {number of colors} {width} {height} {output name}`

In order, with more details:
1. The path to Crochetgen.dll. To generate, download the source and run `dotnet build` in its directory.
2. A filepath to the input image. May be relative, must include file extension (eg .png).
3. The number of colors to simplify the image to. Nothing goes wrong if this number ends up higher than the number of colors actually in the input.
4. The output pattern's width in single stitches.
5. The output pattern's height in single stitch rows.
6. A name for the output files. Should not include an extension, but may be a path. To note: Crochetgen will create and overwrite files, but will not create directories.

Crochetgen uses the [ImageSharp](https://docs.sixlabors.com/articles/imagesharp/index.html) library for direct interfacing with image files.