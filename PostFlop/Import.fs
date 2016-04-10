namespace PostFlop

open System
open Microsoft.Office.Interop.Excel
open System.Runtime.InteropServices
open Options
open Hands

module Import =
  open System.Globalization

  type ExcelOptions = {
    CbetFactor: CBet
    CheckRaise: OnCheckRaise
    Donk: OnDonk
    DonkFlashDraw: OnDonk option
    TurnFVCbetCards: string
    TurnFVCbetFactor: CBet
    TurnCheckRaise: OnCheckRaise
    TurnFBCbetCards: string
    TurnFBCbetFactor: CBet
    TurnFDCbetCards: string
    TurnFDCbetFactor: CBet
  }

  let getCellValue (sheet : Worksheet) (name : string) = 
    let result = sheet.Cells.Range(name, name).Value2 :?> string
    if result = null then "" else result

  let getCellValues (sheet : Worksheet) (name1 : string) (name2 : string) = 
    Console.Write "."
    let toArray (arr:_[,]) = 
      Array.init arr.Length (fun i -> arr.[1, i+1])
    let result = sheet.Cells.Range(name1, name2).Value2 :?> Object[,]
    result
      |> toArray 
      |> Seq.map (fun x -> x |> string)
      |> Seq.map (fun x -> if x = null then "" else x)
      |> Array.ofSeq

  let (|Int|_|) str =
   match System.Int32.TryParse(str) with
   | (true,int) -> Some(int)
   | _ -> None

  let parseInt (s: string) = 
    match s with
    | Int i -> Some i
    | _ -> None

  let (|Decimal|_|) (str: string) =
   match System.Decimal.TryParse(str.Replace(",", "."), NumberStyles.AllowDecimalPoint, CultureInfo.InvariantCulture) with
   | (true,f) -> Some(f)
   | _ -> None

  let parseDecimal (s: string) = 
    match s with
    | Decimal f -> Some f
    | _ -> None

  let parseCBetFV (s: string) =
    match parseDecimal s with
    | Some n -> ForValue n
    | None -> NoCBet

  let parseCBetFB (s: string) =
    match parseDecimal s with
    | Some n -> ForBluff n
    | None -> NoCBet

  let parseCheckRaise (g: string) (h: string) =
    match g.ToLowerInvariant(), h with
    | "stack off", _ -> OnCheckRaise.StackOff
    | "call", _ -> OnCheckRaise.Call
    | "all in", _ -> OnCheckRaise.AllIn
    | "no", Int i -> OnCheckRaise.CallEQ i
    | _ -> OnCheckRaise.Undefined

  let parseDonk (i: string) (j: string) =
    match i.ToLowerInvariant(), j with
    | "fv & stack off", _ -> OnDonk.ForValueStackOff
    | "call/raise + pet", _ -> OnDonk.CallRaisePet
    | "no raise", Int i -> OnDonk.CallEQ i
    | _ -> OnDonk.Undefined

  let parseDonkFD (k: string) =
    match k.ToLowerInvariant() with
    | "stack off" -> Some OnDonk.ForValueStackOff
    | "petarda" -> Some OnDonk.CallRaisePet
    | _ -> None

  let rowIndex : Face seq -> int =
    Seq.map (fun x -> (faceValue x) - 2)
    >> Seq.sort
    >> Seq.mapi (fun i x -> 
      if i = 2 then x 
      else if i = 1 then [13-x..12] |> Seq.sum
      else [13-x..12] |> Seq.map (fun y -> [1..y] |> Seq.sum) |> Seq.sum)
    >> Seq.sum 
    >> (+) 6

  let importOptions (xlWorkBook : Workbook) (hand: Hand) (board: Board) =
    let worksheetName = [hand.Face1; hand.Face2] |> List.map faceToChar |> String.Concat
    let xlWorkSheet = xlWorkBook.Worksheets.[worksheetName] :?> Worksheet
    let index = board |> Seq.take 3 |> Seq.map (fun x -> x.Face) |> rowIndex |> string
    let cellValues = getCellValues xlWorkSheet ("F" + index) ("R" + index)
    { CbetFactor = parseCBetFV cellValues.[0]
      CheckRaise = parseCheckRaise cellValues.[1] cellValues.[2]
      Donk = parseDonk cellValues.[3] cellValues.[4]
      DonkFlashDraw = parseDonkFD cellValues.[5]
      TurnFVCbetCards = cellValues.[6].Replace(" ", "")
      TurnFVCbetFactor = parseCBetFV cellValues.[7]
      TurnCheckRaise = parseCheckRaise cellValues.[8] "100"
      TurnFBCbetCards = cellValues.[9].Replace(" ", "")
      TurnFBCbetFactor = parseCBetFB cellValues.[10]
      TurnFDCbetCards = cellValues.[11].Replace(" ", "")
      TurnFDCbetFactor = parseCBetFV cellValues.[12] }