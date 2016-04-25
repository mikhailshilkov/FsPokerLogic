namespace PostFlop

open System
open Microsoft.Office.Interop.Excel
open System.Runtime.InteropServices
open Options
open Hands
open Cards.HandValues

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

  let parseCBet (s: string) =
    match parseDecimal s with
    | Some n -> Always n
    | None -> Never

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
    let worksheetName = [hand.Face1; hand.Face2] |> List.sortByDescending faceValue |> List.map faceToChar |> String.Concat
    let xlWorkSheet = xlWorkBook.Worksheets.[worksheetName] :?> Worksheet
    let index = board |> Seq.take 3 |> Seq.map (fun x -> x.Face) |> rowIndex |> string
    let cellValues = getCellValues xlWorkSheet ("F" + index) ("R" + index)
    { CbetFactor = parseCBet cellValues.[0]
      CheckRaise = parseCheckRaise cellValues.[1] cellValues.[2]
      Donk = parseDonk cellValues.[3] cellValues.[4]
      DonkFlashDraw = parseDonkFD cellValues.[5]
      TurnFVCbetCards = cellValues.[6].Replace(" ", "")
      TurnFVCbetFactor = 
        match parseDecimal cellValues.[7] with
        | Some n -> OrAllIn { Factor = n; IfStackFactorLessThan = 1.35m; IfPreStackLessThan = 14 }
        | None -> Never 
      TurnCheckRaise = parseCheckRaise cellValues.[8] "100"
      TurnFBCbetCards = cellValues.[9].Replace(" ", "")
      TurnFBCbetFactor =
        match parseDecimal cellValues.[10] with
        | Some n -> OrCheck { Factor = n; IfStackFactorLessThan = 2.8m; IfPreStackLessThan = 18 }
        | None -> Never 
      TurnFDCbetCards = cellValues.[11].Replace(" ", "")
      TurnFDCbetFactor = 
        match parseDecimal cellValues.[12] with
        | Some n -> OrAllIn { Factor = n; IfStackFactorLessThan = 2m; IfPreStackLessThan = 15 }
        | None -> Never 
    }

  let parseTurnDonk (i: string) =
    match i.ToLowerInvariant() with
    | "stack off" -> OnDonk.ForValueStackOff
    | "call" -> OnDonk.Call
    | "fold" -> OnDonk.Fold
    | Int i -> OnDonk.CallEQ i
    | _ -> OnDonk.Undefined

  type TurnSpecialConditions = { 
    StreetyBoard: bool
    DoublePairedBoard: bool }

  let importTurnDonk (xlWorkBook : Workbook) specialConditions handValue =
    let xlWorkSheet = xlWorkBook.Worksheets.["turn vs donkbet"] :?> Worksheet
    let index = 
      (match handValue.Made with
      | StraightFlush | FourOfKind -> 19
      | FullHouse -> 18
      | Flush -> 17
      | Straight(Normal) -> 15
      | Straight(Weak) -> 16
      | ThreeOfKind -> 14
      | TwoPair -> 13
      | Pair(x) -> 
        let highKicker k = k = Ace || k = King || k = Queen || k = Jack
        match x, handValue.FD, handValue.SD with
        | Over, _, _ -> 6 
        | Top(_), Draw, _ -> 31
        | Top(_), _, OpenEnded -> 26
        | Top(_), _, GutShot -> 22
        | Top(k), NoFD, NoSD when highKicker k -> 7
        | Top(_), NoFD, NoSD -> 8
        | Second(_), Draw, _ | Third, Draw, _ | Fourth, Draw, _ -> 32
        | Second(_), _, OpenEnded | Third, _, OpenEnded | Fourth, _, OpenEnded -> 27
        | Second(_), _, GutShot | Third, _, GutShot | Fourth, _, GutShot -> 23
        | Second(k), NoFD, NoSD when highKicker k -> 9
        | Second(_), NoFD, NoSD -> 10
        | Third, NoFD, NoSD | Fourth, NoFD, NoSD -> 11
        | Fifth, _, _ -> failwith "Fifth pair impossible on turn"
        | Under, _, OpenEnded -> 28
        | Under, _, _ -> 12
      | Nothing ->
        match handValue.FD, handValue.SD with
        | Draw, OpenEnded | Draw, GutShot -> 31
        | Draw, NoSD -> 30
        | NoFD, OpenEnded -> 25
        | NoFD, GutShot -> 21
        | NoFD, NoSD -> 4
      )|> string
    let cellValues = getCellValues xlWorkSheet ("B" + index) ("D" + index)
    let specialConditionsApply = 
      [(specialConditions.StreetyBoard, "1"); (specialConditions.DoublePairedBoard, "3")]
      |> List.filter fst
      |> List.map snd
      |> List.exists (fun x -> cellValues.[1].Contains(x))
    parseTurnDonk (if specialConditionsApply then cellValues.[2] else cellValues.[0])