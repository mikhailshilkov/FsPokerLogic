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
  let range = (bb, bb)
  [|{ StackRange = range
      History = []
      Range = getCellValueBB "F3"
      Action = Fold }
    { StackRange = range
      History = []
      Range = getCellValueBB "F5"
      Action = Call }
    { StackRange = range
      History = []
      Range = getCellValueBB "F12"
      Action = MinRaise }
    { StackRange = range
      History = []
      Range = getCellValueBB "F37"
      Action = AllIn } 
    { StackRange = range
      History = [Limp; Raise (0m, 3m)]
      Range = getCellValueBB "F6"
      Action = Fold } 
    { StackRange = range
      History = [Limp; Raise (0m, 3m)]
      Range = getCellValueBB "F7"
      Action = Call } 
    { StackRange = range
      History = [Limp; Raise (0m, 3m)]
      Range = getCellValueBB "F8"
      Action = AllIn } 
    { StackRange = range
      History = [Limp; Raise (3m, 100m)]
      Range = getCellValueBB "F9"
      Action = AllIn } 
    { StackRange = range
      History = [Limp; Raise (3m, 100m)]
      Range = getCellValueBB "F5"
      Action = Fold } 
    { StackRange = range
      History = [Limp; RaiseAllIn]
      Range = getCellValueBB "F10"
      Action = Call } 
    { StackRange = range
      History = [Limp; RaiseAllIn]
      Range = getCellValueBB "F5"
      Action = Fold } 
    { StackRange = range
      History = [Raise (0m, 100m); Raise(0m, 3.35m)]
      Range = getCellValueBB "F14"
      Action = Fold } 
    { StackRange = range
      History = [Raise (0m, 100m); Raise(0m, 3.35m)]
      Range = getCellValueBB "F15"
      Action = Call } 
    { StackRange = range
      History = [Raise (0m, 100m); Raise(0m, 3.35m)]
      Range = getCellValueBB "F16"
      Action = RaiseX 2.5m } 
    { StackRange = range
      History = [Raise (0m, 100m); Raise(0m, 3.35m)]
      Range = getCellValueBB "F17"
      Action = AllIn }
    { StackRange = range
      History = [Raise (0m, 100m); Raise(3.35m, 5.05m)]
      Range = getCellValueBB "F19"
      Action = Fold } 
    { StackRange = range
      History = [Raise (0m, 100m); Raise(3.35m, 5.05m)]
      Range = getCellValueBB "F20"
      Action = Call } 
    { StackRange = range
      History = [Raise (0m, 100m); Raise(3.35m, 5.05m)]
      Range = getCellValueBB "F21"
      Action = RaiseX 2.5m } 
    { StackRange = range
      History = [Raise (0m, 100m); Raise(3.35m, 5.05m)]
      Range = getCellValueBB "F22"
      Action = AllIn } 
    { StackRange = range
      History = [Raise (0m, 100m); Raise(5.05m, 6.55m)]
      Range = getCellValueBB "F24"
      Action = Fold } 
    { StackRange = range
      History = [Raise (0m, 100m); Raise(5.05m, 6.55m)]
      Range = getCellValueBB "F25"
      Action = Call } 
    { StackRange = range
      History = [Raise (0m, 100m); Raise(5.05m, 6.55m)]
      Range = getCellValueBB "F26"
      Action = AllIn } 
    { StackRange = range
      History = [Raise (0m, 100m); Raise(6.55m, 100m)]
      Range = getCellValueBB "F28"
      Action = Fold } 
    { StackRange = range
      History = [Raise (0m, 100m); Raise(6.55m, 100m)]
      Range = getCellValueBB "F29"
      Action = AllIn } 
    { StackRange = range
      History = [Raise (0m, 100m); RaiseAllIn]
      Range = getCellValueBB "F31"
      Action = Fold } 
    { StackRange = range
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
  let range = (bb, bb)
  if bb <= 7 then
     [| { StackRange = range
          History = [Limp]
          Range = getCellValueBB "G3"
          Action = Check } 
        { StackRange = range
          History = [Limp]
          Range = getCellValuesBB ["G4";"G5";"G6";"G7";"G8";"G9"]
          Action = RaiseX 3m } 
        { StackRange = range
          History = [Limp]
          Range = getCellValueBB "G10"
          Action = AllIn } 
        { StackRange = range
          History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G5"
          Action = Fold } 
        { StackRange = range
          History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G6"
          Action = Call } 
        { StackRange = range
          History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G7"
          Action = AllIn } 
        { StackRange = range
          History = [Limp; Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G8"
          Action = Fold } 
        { StackRange = range
          History = [Limp; Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G9"
          Action = Call } 
        { StackRange = range
          History = [Raise(0m, 100m)]
          Range = getCellValueBB "G12"
          Action = Fold } 
        { StackRange = range
          History = [Raise(0m, 100m)]
          Range = getCellValueBB "G13"
          Action = Call } 
        { StackRange = range
          History = [Raise(0m, 100m)]
          Range = getCellValueBB "G14"
          Action = RaiseX 2.5m } 
        { StackRange = range
          History = [Raise(0m, 100m); Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G15"
          Action = AllIn } 
        { StackRange = range
          History = [Raise(0m, 100m); Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G16"
          Action = Call } 
        { StackRange = range
          History = [Raise(0m, 100m)]
          Range = getCellValueBB "G17"
          Action = AllIn } 
        { StackRange = range
          History = [RaiseAllIn]
          Range = getCellValueBB "G19"
          Action = Fold }
        { StackRange = range
          History = [RaiseAllIn]
          Range = getCellValueBB "G20"
          Action = Call }
    |]
  else
     [| { StackRange = range
          History = [Limp]
          Range = getCellValueBB "G3"
          Action = Check } 
        { StackRange = range
          History = [Limp]
          Range = getCellValuesBB ["G4";"G5";"G6";"G7";"G8";"G9"]
          Action = RaiseX 3m } 
        { StackRange = range
          History = [Limp]
          Range = getCellValueBB "G10"
          Action = AllIn } 
        { StackRange = range
          History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G5"
          Action = Fold } 
        { StackRange = range
          History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G6"
          Action = Call } 
        { StackRange = range
          History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G7"
          Action = AllIn } 
        { StackRange = range
          History = [Limp; Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G8"
          Action = Fold } 
        { StackRange = range
          History = [Limp; Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G9"
          Action = Call }     
        { StackRange = range
          History = [Raise(0m, 2.49999m)]
          Range = getCellValueBB "G12"
          Action = Fold } 
        { StackRange = range
          History = [Raise(0m, 2.49999m)]
          Range = getCellValueBB "G13"
          Action = Call } 
        { StackRange = range
          History = [Raise(0m, 2.49999m)]
          Range = getCellValueBB "G14"
          Action = RaiseX 2.5m } 
        { StackRange = range
          History = [Raise(0m, 2.49999m); Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G15"
          Action = AllIn } 
        { StackRange = range
          History = [Raise(0m, 2.49999m); Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G14"
          Action = Fold } 
        { StackRange = range
          History = [Raise(0m, 2.49999m); Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G16"
          Action = Call } 
        { StackRange = range
          History = [Raise(0m, 2.49999m); Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G14"
          Action = Fold } 
        { StackRange = range
          History = [Raise(0m, 2.49999m)]
          Range = getCellValueBB "G17"
          Action = AllIn } 
        { StackRange = range
          History = [Raise(2.5m, 100m)]
          Range = getCellValueBB "G19"
          Action = Fold } 
        { StackRange = range
          History = [Raise(2.5m, 100m)]
          Range = getCellValueBB "G20"
          Action = Call } 
        { StackRange = range
          History = [Raise(2.5m, 100m)]
          Range = getCellValueBB "G21"
          Action = RaiseX 2.5m } 
        { StackRange = range
          History = [Raise(2.5m, 100m); Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G22"
          Action = AllIn } 
        { StackRange = range
          History = [Raise(2.5m, 100m); Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G21"
          Action = Fold } 
        { StackRange = range
          History = [Raise(2.5m, 100m); Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G23"
          Action = Call } 
        { StackRange = range
          History = [Raise(2.5m, 100m); Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G21"
          Action = Fold } 
        { StackRange = range
          History = [Raise(2.5m, 100m)]
          Range = getCellValueBB "G24"
          Action = AllIn } 
        { StackRange = range
          History = [RaiseAllIn]
          Range = getCellValueBB "G26"
          Action = Fold }
        { StackRange = range
          History = [RaiseAllIn]
          Range = getCellValueBB "G27"
          Action = Call }
      |]

let importRulesByStack importRules xlWorkBook =
  ([1..25]
  |> Seq.map (fun bb -> importRules xlWorkBook bb)
  |> Seq.collect id
  |> List.ofSeq)

let importOopLimpRules (xlWorkBook : Workbook) =
  let xlWorkSheet = xlWorkBook.Worksheets.["limp fold low"] :?> Worksheet
  let cellValues = getCellValues xlWorkSheet "B3" "B16"
  let range = (16, 25)
  [| { StackRange = range
       History = [Limp]
       Range = cellValues.[0]
       Action = Check };
     { StackRange = range
       History = [Limp]
       Range = cellValues.[1]
       Action = RaiseX 3m };
     { StackRange = range
       History = [Limp]
       Range = cellValues.[2]
       Action = AllIn } 
     { StackRange = range
       History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
       Range = cellValues.[8]
       Action = AllIn }
     { StackRange = range
       History = [Limp; Raise(0m, 100m); RaiseAllIn]
       Range = cellValues.[9]
       Action = Call }
     { StackRange = range
       History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
       Range = cellValues.[10]
       Action = Call }
     { StackRange = range
       History = [Limp; Raise(0m, 100m); RaiseEQ(34)]
       Range = cellValues.[11]
       Action = Call }
  |]

let importRuleFromExcel importAllRules fileName =
  let xlApp = new ApplicationClass()
  let xlWorkBook = xlApp.Workbooks.Open(fileName)
  let res = importAllRules xlWorkBook

  let misValue = System.Reflection.Missing.Value
  xlWorkBook.Close(false, misValue, misValue)
  Marshal.ReleaseComObject(xlWorkBook) |> ignore
  xlApp.Quit()
  Marshal.ReleaseComObject(xlApp) |> ignore
  res