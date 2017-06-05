namespace Excel

open System

type IWorksheet =
  abstract member GetCellValue: string -> string
  abstract member GetCellValues: string -> string -> string[]  

type IWorkbook =
  abstract member GetWorksheet: string -> IWorksheet

type MemoryWorksheet(sheet: ExcelSheet) =
    let (|Integer|_|) str =
      match System.Int32.TryParse(str) with
      | (true,int) -> Some(int)
      | _ -> None

    let excelColumns = ["A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";"N";"O";"P";"Q";"R";"S";"T";"U";"V";"W";"X";"Y";"Z";"AA";"AB";"AC";"AD";"AE";"AF";"AG";"AH";"AI";"AJ";"AK";"AL";"AM";"AN";"AO";"AP";"AQ";"AR";"AS";"AT";"AU";"AV";"AW";"AX";"AY";"AZ";"BA";"BB";"BC";"BD";"BE";"BF";"BG";"BH";"BI";"BJ";"BK";"BL";"BM";"BN";"BO";"BP";"BQ";"BR";"BS";"BT";"BU";"BV";"BW";"BX";"BY";"BZ";"CA";"CB";"CC";"CD";"CE";"CF";"CG";"CH";"CI";"CJ";"CK";"CL";"CM";"CN";"CO";"CP";"CQ";"CR";"CS";"CT";"CU";"CV";"CW";"CX";"CY";"CZ"]

    let parseCellAddress (cell: string) =
      let column = cell |> Seq.takeWhile (Char.IsLetter) |> String.Concat
      let row = cell |> Seq.skipWhile (Char.IsLetter) |> String.Concat |> int
      let columnIndex = 1 + (excelColumns |> List.findIndex (fun x -> x = column))
      (row, columnIndex)
   
    let enumerateCells (min: string) (max: string) = 
      let (minRow, minCol) = parseCellAddress min
      let (maxRow, maxCol) = parseCellAddress max
      if minRow = maxRow then
        seq { for col in minCol..maxCol do
                yield (sprintf "%i %i" minRow col) }
      else
        seq { for row in minRow..maxRow do
                yield (sprintf "%i %i" row minCol) }

    let getCellValue x = if sheet.Cells.ContainsKey x then sheet.Cells.[x] else ""

    interface IWorksheet with
        
        member this.GetCellValue x = 
          let (row, col) = parseCellAddress x
          getCellValue (sprintf "%i %i" row col)

        member this.GetCellValues x y = 
          enumerateCells x y
          |> Seq.map getCellValue
          |> Array.ofSeq

type MemoryWorkbook(book: ExcelBook) =
    interface IWorkbook with
        member this.GetWorksheet x = 
          new MemoryWorksheet(book.Sheets.[x]) :> IWorksheet

type MemoryWorkstore(database: ExcelDatabase) =
    member this.GetWorkbook x = 
      new MemoryWorkbook(database.Files.[x]) :> IWorkbook
