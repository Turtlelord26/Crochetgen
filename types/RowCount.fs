namespace Crochetgen.RowCount

open Crochetgen.PixelCount
open Crochetgen.PixelCount.Utils

type RowCount = 
    { count: int; 
      rowColors: seq<PixelCount> }

module Utils =

    let makeRowCount pixels = 
        { count = 1; 
          rowColors = pixels }

    let incrementRowCount rowCount = 
        { count = rowCount.count + 1; 
          rowColors = rowCount.rowColors }

    let compareRowCountColors row1 row2 = 
        Seq.compareWith comparePixelCounts row1.rowColors row2.rowColors
    
    let rowCountColors rowCount =
        rowCount.rowColors
    
    let rowCountCount rowCount =
        rowCount.count