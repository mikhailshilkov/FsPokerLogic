module Import

open System
open Microsoft.Office.Interop.Excel
open Preflop
open System.Runtime.InteropServices

let getCellValue (sheet : Worksheet) (name : string) = 
  let result = sheet.Cells.Range(name, name).Value2 :?> string
  if result = null then "" else result

let getCellValues (sheet : Worksheet) (name1 : string) (name2 : string) = 
  Console.Write "."
  let toArray (arr:_[,]) = 
    Array.init arr.Length (fun i -> arr.[i+1, 1])
  let result = sheet.Cells.Range(name1, name2).Value2 :?> Object[,]
  result 
    |> toArray 
    |> Seq.cast<string> 
    |> Seq.map (fun x -> if x = null then "" else x)
    |> Array.ofSeq

let importRulesIP (xlWorkBook : Workbook) bb =
  let xlWorkSheet = xlWorkBook.Worksheets.[bb.ToString() + "BB"] :?> Worksheet
  let cValues = getCellValues xlWorkSheet "C1" "C3"
  let fValues = getCellValues xlWorkSheet "F1" "F37"
  let getCellValueBB (name : string)= 
    let index = name.Substring(1) |> Int32.Parse
    if name.[0] = 'C' then cValues.[index - 1] else fValues.[index - 1]
  [|{ Stack = bb
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
      Action = RaiseX 2.5m } 
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
      Action = RaiseX 2.5m } 
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

let importRulesOOP (xlWorkBook : Workbook) bb =
  let xlWorkSheet = xlWorkBook.Worksheets.[bb.ToString() + "BB"] :?> Worksheet
  let gValues = getCellValues xlWorkSheet "G1" "G27"
  let getCellValueBB (name : string)= 
    let index = name.Substring(1) |> Int32.Parse
    gValues.[index - 1]
  let getCellValuesBB names = 
    String.Join(",", names 
                     |> Seq.map (fun name -> (getCellValueBB name).Replace(" ", "")) 
                     |> Seq.filter (fun x -> x <> ""))
  if bb <= 7 then
     [| { Stack = bb
          History = [Limp]
          Range = getCellValueBB "G3"
          Action = Check } 
        { Stack = bb
          History = [Limp]
          Range = getCellValuesBB ["G4";"G5";"G6";"G7";"G8";"G9"]
          Action = RaiseX 3m } 
        { Stack = bb
          History = [Limp]
          Range = getCellValueBB "G10"
          Action = AllIn } 
        { Stack = bb
          History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G5"
          Action = Fold } 
        { Stack = bb
          History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G6"
          Action = Call } 
        { Stack = bb
          History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G7"
          Action = AllIn } 
        { Stack = bb
          History = [Limp; Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G8"
          Action = Fold } 
        { Stack = bb
          History = [Limp; Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G9"
          Action = Call } 
        { Stack = bb
          History = [Raise(0m, 100m)]
          Range = getCellValueBB "G12"
          Action = Fold } 
        { Stack = bb
          History = [Raise(0m, 100m)]
          Range = getCellValueBB "G13"
          Action = Call } 
        { Stack = bb
          History = [Raise(0m, 100m)]
          Range = getCellValueBB "G14"
          Action = RaiseX 2.5m } 
        { Stack = bb
          History = [Raise(0m, 100m); Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G15"
          Action = AllIn } 
        { Stack = bb
          History = [Raise(0m, 100m); Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G16"
          Action = Call } 
        { Stack = bb
          History = [Raise(0m, 100m)]
          Range = getCellValueBB "G17"
          Action = AllIn } 
        { Stack = bb
          History = [RaiseAllIn]
          Range = getCellValueBB "G19"
          Action = Fold }
        { Stack = bb
          History = [RaiseAllIn]
          Range = getCellValueBB "G20"
          Action = Call }
    |]
  else
     [| { Stack = bb
          History = [Limp]
          Range = getCellValueBB "G3"
          Action = Check } 
        { Stack = bb
          History = [Limp]
          Range = getCellValuesBB ["G4";"G5";"G6";"G7";"G8";"G9"]
          Action = RaiseX 3m } 
        { Stack = bb
          History = [Limp]
          Range = getCellValueBB "G10"
          Action = AllIn } 
        { Stack = bb
          History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G5"
          Action = Fold } 
        { Stack = bb
          History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G6"
          Action = Call } 
        { Stack = bb
          History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G7"
          Action = AllIn } 
        { Stack = bb
          History = [Limp; Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G8"
          Action = Fold } 
        { Stack = bb
          History = [Limp; Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G9"
          Action = Call }     
        { Stack = bb
          History = [Raise(0m, 2.49999m)]
          Range = getCellValueBB "G12"
          Action = Fold } 
        { Stack = bb
          History = [Raise(0m, 2.49999m)]
          Range = getCellValueBB "G13"
          Action = Call } 
        { Stack = bb
          History = [Raise(0m, 2.49999m)]
          Range = getCellValueBB "G14"
          Action = RaiseX 2.5m } 
        { Stack = bb
          History = [Raise(0m, 2.49999m); Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G15"
          Action = AllIn } 
        { Stack = bb
          History = [Raise(0m, 2.49999m); Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G16"
          Action = Call } 
        { Stack = bb
          History = [Raise(0m, 2.49999m)]
          Range = getCellValueBB "G17"
          Action = AllIn } 
        { Stack = bb
          History = [Raise(2.5m, 100m)]
          Range = getCellValueBB "G19"
          Action = Fold } 
        { Stack = bb
          History = [Raise(2.5m, 100m)]
          Range = getCellValueBB "G20"
          Action = Call } 
        { Stack = bb
          History = [Raise(2.5m, 100m)]
          Range = getCellValueBB "G21"
          Action = RaiseX 2.5m } 
        { Stack = bb
          History = [Raise(2.5m, 100m); Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G22"
          Action = AllIn } 
        { Stack = bb
          History = [Raise(2.5m, 100m); Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G23"
          Action = Call } 
        { Stack = bb
          History = [Raise(2.5m, 100m)]
          Range = getCellValueBB "G24"
          Action = AllIn } 
        { Stack = bb
          History = [RaiseAllIn]
          Range = getCellValueBB "G26"
          Action = Fold }
        { Stack = bb
          History = [RaiseAllIn]
          Range = getCellValueBB "G27"
          Action = Call }
      |]


let importRuleFromExcel importRules fileName =
  let xlApp = new ApplicationClass()
  let xlWorkBook = xlApp.Workbooks.Open(fileName)
  let res = ([1..25]
    |> Seq.map (fun bb -> importRules xlWorkBook bb)
    |> Seq.collect id
    |> List.ofSeq)

  Marshal.ReleaseComObject(xlWorkBook) |> ignore
  xlApp.Quit()
  Marshal.ReleaseComObject(xlApp) |> ignore
  res