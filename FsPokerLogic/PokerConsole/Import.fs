module Import

open System
open Microsoft.Office.Interop.Excel
open Preflop

let importRuleFromExcel fileName =
  let xlApp = new ApplicationClass()
  let xlWorkBook = xlApp.Workbooks.Open(fileName)
  let getCellValue (sheet : Worksheet) (name : string) = 
    let result = sheet.Cells.Range(name, name).Value2 :?> string
    if result = null then "" else result

  let importRules bb =
    let xlWorkSheet = xlWorkBook.Worksheets.[bb.ToString() + "BB"] :?> Worksheet
    let getCellValueBB name = 
      Console.Write "."
      getCellValue xlWorkSheet name
    if bb <= 12 then
      [| { Stack = bb
           History = []
           Range = getCellValueBB "C3"
           Action = Fold } 
         { Stack = bb
           History = []
           Range = getCellValueBB "C1"
           Action = AllIn }
      |]
    else
      [| { Stack = bb
           History = []
           Range = getCellValueBB "F3"
           Action = Fold }
         { Stack = bb
           History = []
           Range = getCellValueBB "F5"
           Action = Call }
         { Stack = bb
           History = []
           Range = getCellValueBB "F12"
           Action = MinRaise }
         { Stack = bb
           History = []
           Range = getCellValueBB "F37"
           Action = AllIn } 
         { Stack = bb
           History = [Limp; Raise (0m, 3m)]
           Range = getCellValueBB "F7"
           Action = Fold } 
         { Stack = bb
           History = [Limp; Raise (0m, 3m)]
           Range = getCellValueBB "F8"
           Action = Call } 
         { Stack = bb
           History = [Limp; Raise (3m, 100m)]
           Range = getCellValueBB "F5"
           Action = Fold } 
         { Stack = bb
           History = [Raise (0m, 100m); Raise(0m, 3.35m)]
           Range = getCellValueBB "F14"
           Action = Fold } 
         { Stack = bb
           History = [Raise (0m, 100m); Raise(0m, 3.35m)]
           Range = getCellValueBB "F15"
           Action = Call } 
         { Stack = bb
           History = [Raise (0m, 100m); Raise(0m, 3.35m)]
           Range = getCellValueBB "F16"
           Action = MinRaise } 
         { Stack = bb
           History = [Raise (0m, 100m); Raise(0m, 3.35m)]
           Range = getCellValueBB "F17"
           Action = AllIn }
         { Stack = bb
           History = [Raise (0m, 100m); Raise(3.35m, 5.05m)]
           Range = getCellValueBB "F19"
           Action = Fold } 
         { Stack = bb
           History = [Raise (0m, 100m); Raise(3.35m, 5.05m)]
           Range = getCellValueBB "F20"
           Action = Call } 
         { Stack = bb
           History = [Raise (0m, 100m); Raise(3.35m, 5.05m)]
           Range = getCellValueBB "F21"
           Action = MinRaise } 
         { Stack = bb
           History = [Raise (0m, 100m); Raise(3.35m, 5.05m)]
           Range = getCellValueBB "F22"
           Action = AllIn } 
         { Stack = bb
           History = [Raise (0m, 100m); Raise(5.05m, 6.55m)]
           Range = getCellValueBB "F24"
           Action = Fold } 
         { Stack = bb
           History = [Raise (0m, 100m); Raise(5.05m, 6.55m)]
           Range = getCellValueBB "F25"
           Action = Call } 
         { Stack = bb
           History = [Raise (0m, 100m); Raise(5.05m, 6.55m)]
           Range = getCellValueBB "F26"
           Action = AllIn } 
         { Stack = bb
           History = [Raise (0m, 100m); Raise(6.55m, 100m)]
           Range = getCellValueBB "F28"
           Action = Fold } 
         { Stack = bb
           History = [Raise (0m, 100m); Raise(6.55m, 100m)]
           Range = getCellValueBB "F29"
           Action = AllIn } 
         { Stack = bb
           History = [Raise (0m, 100m); RaiseAllIn]
           Range = getCellValueBB "F31"
           Action = Fold } 
         { Stack = bb
           History = [Raise (0m, 100m); RaiseAllIn]
           Range = getCellValueBB "F35"
           Action = Call } 
      |]


  [1..25]
  |> Seq.map (fun bb -> importRules bb)
  |> Seq.collect id