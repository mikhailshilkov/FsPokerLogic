module Import

open System
open Microsoft.Office.Interop.Excel
open Preflop
open System.Runtime.InteropServices
open Excel.Import

let fullRange = "22+,A2s+,K2s+,Q2s+,J2s+,T2s+,92s+,82s+,72s+,62s+,52s+,42s+,32s,A2o+,K2o+,Q2o+,J2o+,T2o+,92o+,82o+,72o+,62o+,52o+,42o+,32o"

let parseDouble (s: string) =
  let s2 = s.Replace(",", ".")
  System.Decimal.Parse(s2, System.Globalization.CultureInfo.InvariantCulture)

let getCellValue (sheet : Worksheet) (name : string) = 
  let result = sheet.Cells.Range(name, name).Value2 :?> string
  if result = null then "" else result

let getCellValues (sheet : Worksheet) (name1 : string) (name2 : string) = 
  Console.Write "."
  let result = sheet.Cells.Range(name1, name2).Value2 :?> Object[,]
  result 
    |> excelRangeToArray 
    |> Seq.map (fun x -> if x = null then "" else x.ToString())
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
  else if bb = 15 then
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
          Range = getCellValueBB "G27"
          Action = Call }
        { StackRange = range
          History = [RaiseAllIn]
          Range = getCellValueBB "G26"
          Action = Fold }
    |]
  else if bb >= 16 then
    [|  { StackRange = range
          History = [Raise(2.5m, 4m)]
          Range = getCellValueBB "G19"
          Action = Fold } 
        { StackRange = range
          History = [Raise(2.5m, 4m)]
          Range = getCellValueBB "G20"
          Action = Call } 
        { StackRange = range
          History = [Raise(2.5m, 4m)]
          Range = getCellValueBB "G21"
          Action = RaiseX 2.5m } 
        { StackRange = range
          History = [Raise(2.5m, 4m); Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G22"
          Action = AllIn } 
        { StackRange = range
          History = [Raise(2.5m, 4m); Raise(0m, 100m); Raise(0m, 100m)]
          Range = getCellValueBB "G21"
          Action = Fold } 
        { StackRange = range
          History = [Raise(2.5m, 4m); Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G23"
          Action = Call } 
        { StackRange = range
          History = [Raise(2.5m, 4m); Raise(0m, 100m); RaiseAllIn]
          Range = getCellValueBB "G21"
          Action = Fold } 
        { StackRange = range
          History = [Raise(2.5m, 4m)]
          Range = getCellValueBB "G24"
          Action = AllIn } 
        { StackRange = range
          History = [Raise(4m, 100m)]
          Range = getCellValueBB "G27"
          Action = AllIn }
        { StackRange = range
          History = [Raise(4m, 100m)]
          Range = getCellValueBB "G26"
          Action = Fold }
        { StackRange = range
          History = [RaiseAllIn]
          Range = getCellValueBB "G27"
          Action = Call }
        { StackRange = range
          History = [RaiseAllIn]
          Range = getCellValueBB "G26"
          Action = Fold }
    |]
  else // 8 - 14
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

type OopAdvancedRules = {
  Always: DecisionRule list
  LimpFoldLow: DecisionRule list
  LimpFoldBig: DecisionRule list
}

let importOopAdvanced (xlWorkBook : Workbook) =
  let xlWorkSheetLimpLow = xlWorkBook.Worksheets.["limp fold low"] :?> Worksheet
  let cellValuesLimpLow = getCellValues xlWorkSheetLimpLow "B3" "B16"
  let rangeLimp = (16, 25)

  let xlWorkSheetLimpBig = xlWorkBook.Worksheets.["limp fold big"] :?> Worksheet
  let cellValuesLimpBig = getCellValues xlWorkSheetLimpBig "B3" "B27"

  let xlWorkSheetRaise = xlWorkBook.Worksheets.["preflop ranges vs minr"] :?> Worksheet
  let cellValuesRaise = getCellValues xlWorkSheetRaise "B1" "B3"
  let rangeRaise = (15, 25)

  let xlWorkSheetCallingRange = xlWorkBook.Worksheets.["EQ 3b shove - default, formula"] :?> Worksheet
  let cellValuesCallingRange = getCellValues xlWorkSheetCallingRange "B12" "B16"

  { Always = Array.concat
              [|
                [0..4]
                |> Seq.map (fun i -> 
                  let r = [(23, 25); (20, 22); (18, 19); (16, 17); (14, 15)].[i]
                  let sheetName = sprintf "%i - %i bb" (snd r) (fst r)
                  let sheet = xlWorkBook.Worksheets.[sheetName] :?> Worksheet
                  let cellValuesBbThresholds = getCellValues sheet "A1" "A37"
                  let cellValuesBbRanges = getCellValues sheet "B1" "B37"
                  [0..36]
                  |> Seq.map (fun row ->
                    { StackRange = r
                      History = [RaiseFor3BetShove(parseDouble cellValuesCallingRange.[i], parseDouble cellValuesBbThresholds.[row])]
                      Range = cellValuesBbRanges.[row]
                      Action = AllIn })
                  |> Array.ofSeq)
                |> Array.concat;

                [|
                 { StackRange = rangeRaise
                   History = [Raise(0m, 2.5m)]
                   Range = cellValuesRaise.[0]
                   Action = RaiseX 2.5m }
                 { StackRange = rangeRaise
                   History = [Raise(0m, 2.5m); Raise(0m, 100m); Raise(0m, 100m)]
                   Range = cellValuesRaise.[0]
                   Action = AllIn }
                 { StackRange = rangeRaise
                   History = [Raise(0m, 2.5m); Raise(0m, 100m); RaiseAllIn]
                   Range = cellValuesRaise.[0]
                   Action = Call }
                 { StackRange = (20, 25)
                   History = [BluffableRaise]
                   Range = cellValuesRaise.[1]
                   Action = RaiseBluffX 2.5m }
                 { StackRange = rangeRaise
                   History = [Raise(0m, 2.5m)]
                   Range = cellValuesRaise.[2]
                   Action = Call }
                 { StackRange = rangeRaise
                   History = [Raise(0m, 2.5m)]
                   Range = fullRange
                   Action = Fold }
                 { StackRange = rangeRaise
                   History = [Raise(0m, 2.5m); Raise(0m, 100m); Raise(0m, 100m)]
                   Range = fullRange
                   Action = Fold }
                 { StackRange = rangeRaise
                   History = [Raise(0m, 2.5m); Raise(0m, 100m); RaiseAllIn]
                   Range = fullRange
                   Action = Fold }
                |]
              |]
            |> List.ofArray
    LimpFoldLow = 
              [ 
                { StackRange = rangeLimp
                  History = [Limp]
                  Range = cellValuesLimpLow.[0]
                  Action = Check };
                { StackRange = rangeLimp
                  History = [Limp]
                  Range = cellValuesLimpLow.[1]
                  Action = RaiseX 3m };
                { StackRange = rangeLimp
                  History = [Limp]
                  Range = cellValuesLimpLow.[2]
                  Action = AllIn } 
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
                  Range = cellValuesLimpLow.[8]
                  Action = AllIn }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); RaiseAllIn]
                  Range = cellValuesLimpLow.[9]
                  Action = Call }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
                  Range = cellValuesLimpLow.[10]
                  Action = Call }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); RaiseEQ(34)]
                  Range = cellValuesLimpLow.[11]
                  Action = Call }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); RaiseEQ(30)]
                  Range = cellValuesLimpLow.[12]
                  Action = Call }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); RaiseEQ(25)]
                  Range = cellValuesLimpLow.[13]
                  Action = Call }
                { StackRange = rangeLimp
                  History = [Limp]
                  Range = fullRange
                  Action = Fold }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
                  Range = fullRange
                  Action = Fold }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); RaiseAllIn]
                  Range = fullRange
                  Action = Fold }
              ]
    LimpFoldBig = 
              [ 
                { StackRange = rangeLimp
                  History = [Limp]
                  Range = cellValuesLimpBig.[0]
                  Action = Check };
                { StackRange = rangeLimp
                  History = [Limp]
                  Range = cellValuesLimpBig.[1]
                  Action = RaiseX 3m };
                { StackRange = rangeLimp
                  History = [Limp]
                  Range = cellValuesLimpBig.[2]
                  Action = RaiseBluffX 3m };
                { StackRange = rangeLimp
                  History = [Limp]
                  Range = cellValuesLimpBig.[3]
                  Action = AllIn } 
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
                  Range = cellValuesLimpBig.[9]
                  Action = AllIn }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); RaiseAllIn]
                  Range = cellValuesLimpBig.[10]
                  Action = Call }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
                  Range = cellValuesLimpBig.[11]
                  Action = Call }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); RaiseEQ(34)]
                  Range = cellValuesLimpBig.[12]
                  Action = Call }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); RaiseEQ(30)]
                  Range = cellValuesLimpBig.[13]
                  Action = Call }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); RaiseEQ(25)]
                  Range = cellValuesLimpBig.[14]
                  Action = Call }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); RaiseEQ(22)]
                  Range = cellValuesLimpBig.[15]
                  Action = Call }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); RaiseEQ(30)]
                  Range = cellValuesLimpBig.[21]
                  Action = Call }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); RaiseEQ(28)]
                  Range = cellValuesLimpBig.[22]
                  Action = Call }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); RaiseEQ(25)]
                  Range = cellValuesLimpBig.[23]
                  Action = Call }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); RaiseEQ(22)]
                  Range = cellValuesLimpBig.[24]
                  Action = Call }
                { StackRange = rangeLimp
                  History = [Limp]
                  Range = fullRange
                  Action = Fold }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); Raise(0m, 100m)]
                  Range = fullRange
                  Action = Fold }
                { StackRange = rangeLimp
                  History = [Limp; Raise(0m, 100m); RaiseAllIn]
                  Range = fullRange
                  Action = Fold }
              ]
  }

let importHudData (xlWorkBook : Workbook) =
  let xlWorkSheet = xlWorkBook.Worksheets.["Hud"] :?> Worksheet
  [3..10000]
  |> Seq.map (fun row -> getCellValues xlWorkSheet ("A" + row.ToString()) ("E" + row.ToString()))
  |> Seq.takeWhile (fun vs -> not(String.IsNullOrEmpty(vs.[0])))
  |> Seq.map (fun vs ->  
     { VillainName = vs.[0] 
       OpenRaise20_25 = Int32.Parse(vs.[1]) 
       OpenRaise16_19 = Int32.Parse(vs.[2]) 
       OpenRaise14_15 = Int32.Parse(vs.[3])
       LimpFold = Int32.Parse(vs.[4]) }
     )
  |> List.ofSeq
