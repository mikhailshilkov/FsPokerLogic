namespace Excel

open Microsoft.Office.Interop.Excel
open System
open System.Runtime.Serialization
open System.Collections.Generic
open System.IO
open System.IO.Compression
open Import

[<DataContract>]
type ExcelSheet = {
    [<field: DataMember >]
    Cells:IDictionary<string, string> }

[<DataContract>]
type ExcelBook = {
    [<field: DataMember >]
    Sheets:IDictionary<string, ExcelSheet> }

[<DataContract>]
type ExcelDatabase = {
    [<field: DataMember >]
    Files:IDictionary<string, ExcelBook> }

module Serialization =

  let parseWorksheet (workSheet: Worksheet) =
      //These two lines do the magic.
      workSheet.Columns.ClearFormats() |> ignore
      workSheet.Rows.ClearFormats() |> ignore

      let cells = workSheet.UsedRange.Value2 :?> Object[,]
      match cells with
      | null -> None
      | _ ->
        let values =
          seq {
            for row in 1 .. Array2D.length1 cells do
              for column in 1 .. Array2D.length2 cells do
                let elem = cells.[row, column]
                if elem <> null then 
                  let str = elem.ToString()
                  if String.IsNullOrEmpty str |> not then
                    yield (sprintf "%i %i" row column, str) }
          |> dict
        Some { Cells = values }

  let parseExcel (fileName: string) =
      use xlTricky = useExcel fileName
      let sheets =
        seq {
          for sheet in 1 .. xlTricky.Workbook.Worksheets.Count do
            let worksheet = xlTricky.Workbook.Worksheets.[sheet] :?> Worksheet
            let parsed = parseWorksheet worksheet
            match parsed with
            | Some p -> yield worksheet.Name, p 
            | None -> () }
        |> dict
      { Sheets = sheets }

  let loadRules () =
    let serializer = new DataContractSerializer(typeof<ExcelDatabase>)
    let filename =
      Directory.GetFiles(System.IO.Directory.GetCurrentDirectory(), "rules*.zip")
      |> Array.head
    use file = File.OpenRead filename
    use archive = new ZipArchive(file, ZipArchiveMode.Read)
    use entry = archive.Entries.[0].Open()    
    serializer.ReadObject entry :?> ExcelDatabase