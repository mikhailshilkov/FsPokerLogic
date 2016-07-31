namespace PostFlop

open System
open Microsoft.Office.Interop.Excel
open System.Runtime.InteropServices
open Options
open Hands
open Cards.Actions
open Cards.HandValues
open Excel.Import
open Decision

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
  }

  let getCellValue (sheet : Worksheet) (name : string) = 
    let result = sheet.Cells.Range(name, name).Value2 |> string
    if result = null then "" else result

  let getCellValues (sheet : Worksheet) (name1 : string) (name2 : string) = 
    Console.Write "."
    let result = sheet.Cells.Range(name1, name2).Value2 :?> Object[,]
    result
      |> excelRangeToArray 
      |> Seq.map (fun x -> x |> string)
      |> Seq.map (fun x -> if x = null then "" else x)
      |> Array.ofSeq

  let (|Int|_|) str =
   match System.Int32.TryParse(str) with
   | (true,int) -> Some(int)
   | _ -> None

  let (|StartsWith|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        s.Substring(p.Length) |> Some
    else
        None

  let parseInt (s: string) = 
    match s with
    | Int i -> Some i
    | _ -> None

  let (|Decimal|_|) (str: string) =
   match System.Decimal.TryParse(str.Replace(",", "."), NumberStyles.AllowDecimalPoint, CultureInfo.InvariantCulture) with
   | (true,f) -> Some(f)
   | _ -> None

  let (|DecimalPerc|_|) (str: string) =
   if str.EndsWith("%") then match str.Replace("%", "") with | Decimal d -> Some d | _ -> None else None

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

  let importOptions (xlWorkBook : Workbook) (hand: SuitedHand) (board: Board) limpedPot =
    let h = toHand hand
    let worksheetName = [h.Face1; h.Face2] |> List.sortByDescending faceValue |> List.map faceToChar |> String.Concat
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
        | Some n -> OrAllIn { DefaultCBetOr with Factor = n; IfPreStackLessThan = if limpedPot then 8 else 14 }
        | None -> Never 
      TurnCheckRaise = parseCheckRaise cellValues.[8] "100"
      TurnFBCbetCards = cellValues.[9].Replace(" ", "")
      TurnFBCbetFactor =
        match parseDecimal cellValues.[10] with
        | Some n -> OrAllIn { DefaultCBetOr with Factor = n; IfPreStackLessThan = if limpedPot then 8 else 14 }
        | None -> Never 
    }

  let parseTurnRiverDonk (i: string) =
    match i.ToLowerInvariant() with
    | "stack off" -> (OnDonk.ForValueStackOffX 250, OnDonkRaise.StackOff)
    | "call" -> (OnDonk.Call, OnDonkRaise.Undefined)
    | "fold" -> (OnDonk.Fold, OnDonkRaise.Undefined)
    | "call/raise" -> (OnDonk.CallRaisePet, OnDonkRaise.Undefined)
    | Int i -> (OnDonk.CallEQ i, OnDonkRaise.Undefined)
    | _ -> (OnDonk.Undefined, OnDonkRaise.Undefined)

  let specialConditionsApply sc (v: string) = 
    [(sc.Streety, "1"); (sc.DoublePaired, "3")]
    |> List.filter fst
    |> List.map snd
    |> List.exists (fun x -> v.Contains(x))

  let parseTurnDonk (strategy: string) =
    let canonic = strategy.Trim().ToLowerInvariant()
    let parts = canonic.Split([|'/'|], 2)
    if System.String.IsNullOrEmpty(strategy) then (OnDonk.Undefined, OnDonkRaise.Undefined)
    else if parts.Length = 1 then
      (match parts.[0] with 
      | "c" -> OnDonk.Call
      | "f" -> OnDonk.Fold
      | "ai" -> OnDonk.AllIn
      | Int n -> OnDonk.CallEQ n
      | _ -> failwith "Failed parsing Turn Donk (1)"
      , OnDonkRaise.Undefined)
    elif parts.Length = 2 then
      let donk = 
        match parts.[0] with 
        | "rtsdb" -> OnDonk.RaisePreDonkX 110
        | "rtbdb" -> OnDonk.RaiseX 260
        | "rtg" -> OnDonk.RaiseGay
        | "rtfb" -> OnDonk.RaiseX 220
        | StartsWith "rmodx" s -> match s with | Decimal x -> OnDonk.RaiseX (x * 100m |> int) | _ -> failwith "Failed parsing Turn Donk (5)"
        | _ -> failwith "Failed parsing Turn Donk (2)"
      let raise =
        match parts.[1] with 
        | "sot" -> OnDonkRaise.StackOff
        | Int n -> OnDonkRaise.CallEQ n
        | _ -> failwith "Failed parsing TurnDonk (3)" 
      (donk, raise)
    else failwith "Failed parsing TurnDonk (4)" 

  let importTurnDonk (xlWorkBook : Workbook) handValue texture s h =
    let historyFlop = h |> List.filter (fun x -> x.Street = Flop)
    let sheetName = 
      match historyFlop with
      | [{ Action = Action.Check }] -> "xx flop + vill bet turn"
      | [{ Action = Action.RaiseToAmount(_); VsVillainBet = 0 }] 
      | [{ Action = Action.Call }] -> "vill xc F + dbT or dbF + dbT"
      | _ when historyFlop |> List.exists (fun x -> x.VsVillainBet > 0) -> "vill xr F + dbT or dbF-c + dbT"
      | _ -> failwith "Could not pick turn donkbet sheet"

    let xlWorkSheet = xlWorkBook.Worksheets.[sheetName] :?> Worksheet
    let isTurnOvercard = isLastBoardCardOvercard s.Board
    let isTurnMiddlecard = isLastBoardCardSecondCard s.Board
    let isTurnAce = (Array.last s.Board).Face = Ace
    let handHasAce = s.Hand.Card1.Face = Ace || s.Hand.Card2.Face = Ace
    let overs = overcards s.Hand s.Board
    let highKicker k = k = Ace || k = King || k = Queen || k = Jack
    let highBoardPair = pairFace s.Board |> Option.filter highKicker |> Option.isSome
    let topPairedBoard = topPaired s.Board
    let isPairedBoard = isPaired s.Board
    let betSize = 100 * s.VillainBet / (s.Pot - s.VillainBet)
    let hasFlush = match handValue.Made with | Flush(_) | StraightFlush -> true | _ -> false
    let noDrawRow = 
      match handValue.Made with
      | Nothing -> 
        if overs = 1 && isTurnOvercard then 13
        elif overs = 1 then 12
        elif handHasAce && isTurnOvercard then 10
        elif handHasAce then 9
        elif isTurnAce then 8 
        elif isTurnOvercard then 7 
        elif isTurnMiddlecard then 11
        else 6
      | TwoOvercards -> if isTurnOvercard then 15 else 14
      | Pair(x) -> 
        match x with
        | Over -> 17
        | Top(k) when highKicker k -> 18
        | Top(_) when highBoardPair -> 20
        | Top(_) -> 19
        | Second(x) when topPairedBoard -> if highKicker x then 21 else 22
        | Second(x) when isPairedBoard -> if highKicker x then 24 else 26
        | Second(x) when isTurnOvercard -> if highKicker x then 27 else 28
        | Second(x) -> if highKicker x then 23 else 25
        | Third when isPairedBoard -> 30
        | Third when isTurnAce -> 32
        | Third when isTurnOvercard -> 31
        | Third -> 29
        | Fourth when isTurnAce -> 34
        | Fourth -> 33
        | Fifth -> failwith "Fifth pair impossible on turn"
        | Under -> 35
      | TwoPair when isPairedBoard -> 37
      | TwoPair -> 37
      | ThreeOfKind -> 38
      | Straight(Normal) -> 39
      | Straight(Weak) -> 40
      | Flush(x) -> 
        if texture.Monoboard = 4 then
          match x with 
          | Nut -> 6
          | NotNut(King) -> 7
          | NotNut(Queen) -> 8
          | NotNut(Jack) -> 9
          | NotNut(Ten) | NotNut(Nine) -> 10
          | NotNut(Eight) | NotNut(Seven) -> 11
          | NotNut(Six) | NotNut(Five) -> 12
          | NotNut(_) -> 13
          | Board -> failwith "Board flush impossible on 4 monoboard"
        else 41
      | FullHouse(x) ->
        match tripsFace s.Board with
        | Some(k) -> 
          let kicker = concat s.Hand s.Board |> anyPairFace |> Option.get
          if highKicker kicker then 44 else 43
        | None -> if x = Normal then 42 else 45
      | FourOfKind -> 
        if texture.FourOfKind then 
          let kicker = maxFace [s.Hand.Card1; s.Hand.Card2]
          let isNuts = kicker = Ace || (kicker = Ace && s.Board.[0].Face = King)
          if isNuts then 102
          elif kicker = King || kicker = Queen || kicker = Jack then 103 else 104 
        else 46
      | StraightFlush -> 46
    let gutShotRow = 
      match handValue.Made with
      | Nothing -> 
        if overs = 1 && isTurnOvercard then 52
        elif overs = 1 then 51
        elif isTurnAce then 50
        elif isTurnOvercard then 49
        else 48
      | TwoOvercards -> if isTurnOvercard then 54 else 53
      | Pair(x) -> 
        match x with
        | Top(_) -> 55
        | Second(x) when topPairedBoard -> 58
        | Second(x) when isPairedBoard -> 59
        | Second(x) when isTurnOvercard -> 57
        | Second(x) -> 56
        | Third when isTurnOvercard -> 61
        | Third -> 60
        | Fourth when isTurnAce -> 63
        | Fourth when isTurnOvercard -> 64
        | Fourth -> 62
        | _ -> 0
      | _ -> 0
    let openEndedRow = 
      match handValue.Made with
      | Nothing -> 
        if overs = 1 && isTurnOvercard then 70
        elif overs = 1 then 69
        elif isTurnAce then 67
        elif isTurnOvercard then 68
        else 66
      | TwoOvercards -> if isTurnOvercard then 72 else 71
      | Pair(x) -> 
        match x with
        | Top(_) -> 73
        | Second(x) when topPairedBoard -> 76
        | Second(x) when isPairedBoard -> 77
        | Second(x) when isTurnOvercard -> 75
        | Second(x) -> 74
        | Third when isTurnOvercard -> 79
        | Third -> 78
        | Fourth when isTurnAce -> 81
        | Fourth -> 80
        | Under -> 82
        | _ -> 0
      | _ -> 0
    let flushDrawRow = 
      match handValue.Made with
      | Nothing -> 
        match handValue.SD with
        | OpenEnded -> 91
        | GutShot when isTurnOvercard -> 93
        | GutShot -> 92
        | NoSD ->
          if overs = 1 && isTurnOvercard then 87
          elif overs = 1 then 86
          elif isTurnOvercard then 85
          else 84
      | TwoOvercards -> if isTurnOvercard then 89 else 88
      | Pair(x) -> 
        match x with
        | Top(_) -> 90
        | Second(x) when topPairedBoard -> 96
        | Second(x) when isPairedBoard -> 97
        | Second(x) when isTurnOvercard -> 95
        | Second(x) -> 94
        | Third when isTurnOvercard -> 99
        | Third -> 98
        | Fourth when isTurnOvercard -> 101
        | Fourth -> 100
        | _ -> 0
      | _ -> 0
    let row =
      (match handValue.FD2, handValue.SD with 
       | Draw(_), _ when texture.Monoboard < 3 && flushDrawRow > 0 -> flushDrawRow
       | NoFD, OpenEnded when openEndedRow > 0 -> openEndedRow
       | NoFD, GutShot when gutShotRow > 0 -> gutShotRow 
       | _ -> noDrawRow
      ) |> string
    let column = 
      if texture.Monoboard = 4 then 
        if hasFlush then 
          if betSize <= 40 then "AF" else "AG"
        else "AC"
      elif texture.Monoboard = 3 then
        match handValue.FD with
        | NoFD ->
          if texture.Streety then "P"
          elif betSize <= 25 then "L" elif betSize < 50 then "M" elif betSize = 50 then "N" else "O"
        | Draw(Nut) -> if texture.Streety then "S" else "R"
        | Draw(NotNut x) when x = King || x = Queen -> if texture.Streety then "U" else "T"
        | Draw(NotNut x) when x = Jack || x = Ten -> if texture.Streety then "W" else "V"
        | Draw(NotNut x) when x = Nine || x = Eight || x = Seven -> if texture.Streety then "Y" else "X"
        | Draw(NotNut x) -> if texture.Streety then "AA" else "Z"
        | Draw(Board) -> failwith "Board draw impossible on 3 monoboard"
      else
        if texture.Streety then "H" elif texture.DoublePaired then "I" 
        elif texture.ThreeOfKind && (match handValue.Made with | FullHouse(_) -> false | _ -> true) then "J" 
        elif betSize <= 25 then "C" elif betSize < 50 then "D" elif betSize = 50 then "E" else "F"
    let cellValue = getCellValue xlWorkSheet (column + row)
    parseTurnDonk cellValue

  let parseRiverCbet (i: string) =
    match i.ToLowerInvariant() with
    | "stack off" -> (OrAllIn { DefaultCBetOr with Factor = 62.5m; IfStackFactorLessThan = Some(4m/3m) }, OnCheckRaise.StackOff)
    | "check" -> (Never, OnCheckRaise.Undefined)
    | Decimal d -> (Always(d), OnCheckRaise.CallEQ 11)
    | _ -> failwith "Failed parsing River on check"

  let importRiver (xlWorkBook : Workbook) specialConditions handValue =
    let xlWorkSheet = xlWorkBook.Worksheets.["river"] :?> Worksheet
    let index = 
      (match handValue with
      | StraightFlush | FourOfKind -> 21
      | FullHouse(Normal) -> 19
      | FullHouse(Weak) -> 20
      | Flush(_) -> 18
      | Straight(Normal) -> 16
      | Straight(Weak) -> 17
      | ThreeOfKind -> 15
      | TwoPair -> 14
      | Pair(x) -> 
        let highKicker k = k = Ace || k = King || k = Queen || k = Jack
        match x with
        | Over -> 5
        | Top(k) when highKicker k -> 6
        | Top(_) -> 7
        | Second(k) when highKicker k -> 8
        | Second(_) -> 9
        | Third -> 10
        | Fourth -> 11
        | Fifth -> 12
        | Under -> 13
      | TwoOvercards | Nothing -> 4
      )|> string
    let cellValues = getCellValues xlWorkSheet ("B" + index) ("G" + index)
    let check = parseRiverCbet (if specialConditionsApply specialConditions cellValues.[1] then cellValues.[2] else cellValues.[0])
    let donk = parseTurnRiverDonk (if specialConditionsApply specialConditions cellValues.[4] then cellValues.[5] else cellValues.[3])
    { Options.CbetFactor = fst check
      CheckRaise = snd check
      Donk = fst donk
      DonkRaise = snd donk }

  let parseOopSpecialRules (specialRules: string) = 
    let parseOopSpecialRule (s: string) =
      match s.Trim().ToLowerInvariant() with
      | "ai" -> CallEQPlusXvsAI 10
      | "ai+" -> CallEQPlusXvsAI 14
      | "bp gs" -> PairedBoard (OopDonk.Check, CallEQ 14)
      | "bp fd" -> PairedBoard (OopDonk.Check, CallEQ 22)
      | "22" -> PairedBoard (Donk 50m, CallEQ 20)
      | "tpp" -> PairedBoard (OopDonk.AllIn, OopOnCBet.AllIn)
      | "6" -> BoardOvercard(OopDonk.Check, OopOnCBet.Call)
      | "ov" -> BoardOvercard(OopDonk.AllIn, OopOnCBet.AllIn)
      | "ov ai" -> BoardOvercard(OopDonk.Check, OopOnCBet.AllIn)
      | "ovso" -> BoardOvercard(Donk 67m, StackOff)
      | "61" -> BoardOvercard(Donk 100m, CallEQ 25)
      | "4" -> BoardOvercard(OopDonk.Check, StackOff)
      | "44" -> BoardOvercard(Donk 62.5m, CallEQ 20)
      | "a" -> BoardAce (OopDonk.AllIn, OopOnCBet.AllIn)
      | "a/f" -> BoardAce(Donk 67m, OopOnCBet.Fold)
      | "aso" -> BoardAce(Donk 67m, StackOff)
      | "5" -> CheckCheck (Donk 75m, OopOnCBet.Call)
      | "7" -> CheckCheck (Donk 75m, StackOff)
      | "ov ch ch" -> CheckCheckAndBoardOvercard (Donk 75m, CallEQ 22)
      | "60" -> KHighOnPaired
      | "1" -> NotUsed
      | _ -> failwith "Failed parsing special rules"
    specialRules.Split(',') 
    |> Array.filter (fun x -> not(String.IsNullOrEmpty(x)))
    |> List.ofArray 
    |> List.map parseOopSpecialRule

  let rec parseOopOption (strategy: string) (specialRules: string) =
    let canonic = strategy.Trim().ToLowerInvariant()
    let specialParts = canonic.Split([|'@'|], 2)
    if specialParts.Length = 2 then parseOopOption specialParts.[0] specialParts.[1]
    else
    let parts = canonic.Split([|'/'|], 2)
    if System.String.IsNullOrEmpty(strategy) then None
    else if parts.Length = 1 then
      match parts.[0] with 
      | "ai" -> Some { First = OopDonk.AllIn; Then = OopOnCBet.AllIn; Special = parseOopSpecialRules specialRules  }
      | "x" -> None
      | _ -> failwith "Failed parsing Flop Oop (1)"
    else if parts.Length = 2 then
      let donk = 
        match parts.[0] with 
        | "ch" -> OopDonk.Check
        | DecimalPerc n -> OopDonk.Donk n
        | _ -> failwith "Failed parsing Flop Oop Donk"
      let cbet =
        match parts.[1] with 
        | StartsWith "r/" p3 -> 
          match p3 with 
          | "f" -> OopOnCBet.RaiseFold(2.75m)
          | "c" -> OopOnCBet.RaiseCall
          | Int i -> OopOnCBet.RaiseCallEQ i
          | _ -> failwith "Failed parsing Flop Oop OnCbet raise"
        | "f" -> OopOnCBet.Fold
        | "so" -> OopOnCBet.StackOff
        | "so+" -> OopOnCBet.StackOffFast
        | "c" -> OopOnCBet.Call
        | "ai" -> OopOnCBet.AllIn
        | Int i -> CallEQ i
        | _ -> failwith "Failed parsing Flop Oop OnCbet" 
      Some { First = donk; Then = cbet; Special = parseOopSpecialRules specialRules  }
    else failwith "Failed parsing Flop Oop (2)"

  let parseOopOptionWithSpecialBoard (strategy: string) isSpecialBoard (specialBoardStrategy:string) (specialRules: string) =
    if isSpecialBoard then
      match parseOopOption specialBoardStrategy specialRules with
      | None -> parseOopOption strategy specialRules
      | x -> x
    else parseOopOption strategy specialRules

  let importOopFlop (xlWorkBook : Workbook) sheetName handValue texture =
    let xlWorkSheet = xlWorkBook.Worksheets.[sheetName] :?> Worksheet
    let index = 
      (match handValue.Made with
      | StraightFlush | FourOfKind -> 21
      | FullHouse(_) -> 20
      | Flush(_) -> 19
      | Straight(_) -> 18
      | ThreeOfKind -> 17
      | TwoPair -> 16
      | Pair(x) -> 
        let highKicker k = k = Ace || k = King || k = Queen || k = Jack
        match x, handValue.FD2, handValue.SD with
        | Over, _, _ -> 9
        | Top(_), Draw(_), _ -> 36
        | Top(_), _, OpenEnded -> 30
        | Top(_), _, GutShot -> 25
        | Top(k), NoFD, NoSD when highKicker k -> 10
        | Top(_), NoFD, NoSD -> 11
        | Second(_), Draw(_), _ | Third, Draw(_), _ -> 38
        | Second(_), _, OpenEnded | Third, _, OpenEnded -> 31
        | Second(_), _, GutShot | Third, _, GutShot -> 26
        | Second(k), NoFD, NoSD when highKicker k -> 12
        | Second(_), NoFD, NoSD -> 13
        | Third, NoFD, NoSD -> 14
        | Fourth, _, _ | Fifth, _, _ -> failwith "Fourth/Fifth pair impossible on flop"
        | Under, _, OpenEnded -> 32
        | Under, _, _ -> 15
      | TwoOvercards ->
        match handValue.FD2, handValue.SD with
        | Draw(_), OpenEnded | Draw(_), GutShot -> 37
        | Draw(_), NoSD -> 35
        | NoFD, OpenEnded -> 29
        | NoFD, GutShot -> 24
        | NoFD, NoSD -> 7
      | Nothing -> 
        match handValue.FD2, handValue.SD with
        | Draw(_), OpenEnded | Draw(_), GutShot -> 37
        | Draw(_), NoSD -> 34
        | NoFD, OpenEnded -> 28
        | NoFD, GutShot -> 23
        | NoFD, NoSD -> 6
      )|> string
    let cellValues = getCellValues xlWorkSheet ("B" + index) ("G" + index)
    let (column, specialRulesColumn) = 
      match texture.Monoboard, handValue.FD with
      | 3, NoFD -> (2, 4)
      | 3, Draw(Nut) | 3, Draw(NotNut(King)) | 3, Draw(NotNut(Queen)) | 3, Draw(NotNut(Jack)) -> (5, 4)
      | 3, Draw(_) -> (3, 4)
      | _ -> (0, 1)
    parseOopOption cellValues.[column] cellValues.[specialRulesColumn]

  let importOopTurn (xlWorkBook : Workbook) sheetName handValue texture =
    let xlWorkSheet = xlWorkBook.Worksheets.[sheetName] :?> Worksheet
    let index = 
      (match handValue.Made with
      | StraightFlush | FourOfKind -> 24
      | FullHouse(Normal) -> 22
      | FullHouse(Weak) -> 23
      | Flush(_) -> 21
      | Straight(Normal) -> 19
      | Straight(Weak) -> 20
      | ThreeOfKind -> 18
      | TwoPair -> 17
      | Pair(x) -> 
        let highKicker k = k = Ace || k = King || k = Queen || k = Jack
        match x, handValue.FD2, handValue.SD with
        | Over, _, _ -> 9
        | Top(_), Draw(_), _ -> 41
        | Top(_), _, OpenEnded -> 34
        | Top(_), _, GutShot -> 28
        | Top(k), NoFD, NoSD when highKicker k -> 10
        | Top(_), NoFD, NoSD -> 11
        | Second(_), Draw(_), _  -> 43
        | Third, Draw(_), _ | Fourth, Draw(_), _ -> 44
        | Second(_), _, OpenEnded -> 35 
        | Third, _, OpenEnded | Fourth, _, OpenEnded -> 36
        | Second(_), _, GutShot -> 29 
        | Third, _, GutShot | Fourth, _, GutShot -> 30
        | Second(k), NoFD, NoSD when highKicker k -> 12
        | Second(_), NoFD, NoSD -> 13
        | Third, NoFD, NoSD -> 14
        | Fourth, _, _ ->   15
        | Fifth, _, _ -> failwith "Fifth pair impossible on егкт"
        | Under, _, OpenEnded -> 37
        | Under, _, _ -> 16
      | TwoOvercards ->
        match handValue.FD2, handValue.SD with
        | Draw(_), OpenEnded | Draw(_), GutShot -> 42
        | Draw(_), NoSD -> 40
        | NoFD, OpenEnded -> 33
        | NoFD, GutShot -> 27
        | NoFD, NoSD -> 7
      | Nothing -> 
        match handValue.FD2, handValue.SD with
        | Draw(_), OpenEnded | Draw(_), GutShot -> 42
        | Draw(_), NoSD -> 39
        | NoFD, OpenEnded -> 32
        | NoFD, GutShot -> 26
        | NoFD, NoSD -> 6
      )|> string
    let cellValues = getCellValues xlWorkSheet ("K" + index) ("AB" + index)

    let specialConditionsColumn = 
      match texture.Monoboard with
      | 4 -> 13
      | 3 -> 10
      | _ -> 2
    let sc = specialConditionsApply texture cellValues.[specialConditionsColumn]

    let (column, specialColumn) = 
      match texture.Monoboard, handValue.FD with
      | 4, _ ->
        match handValue.Made with
        | Flush(Nut) | Flush(NotNut King) | Flush(NotNut Queen) | Flush(NotNut Jack) -> (16, 17)
        | Flush(_) -> (14, 15)
        | _ -> (11, 12)
      | 3, NoFD -> (4, 5)
      | 3, Draw(Nut) | 3, Draw(NotNut(King)) | 3, Draw(NotNut(Queen)) | 3, Draw(NotNut(Jack)) -> (8, 9)
      | 3, Draw(_) -> (6, 7)
      | _ -> (0, 3)
    let specialRules = if texture.Monoboard < 3 then cellValues.[1] else "" // monoboard rules are in the cell itself after @ sign
    parseOopOptionWithSpecialBoard cellValues.[column] sc cellValues.[specialColumn] specialRules

  let importOopRiver (xlWorkBook : Workbook) sheetName handValue texture =
    let defaultMapping () =
      let index = 
        (match handValue with
        | StraightFlush | FourOfKind ->
          match texture.Monoboard with
          | 4 -> 30
          | 5 -> 36
          | _ -> 23
        | FullHouse(Normal) -> 21
        | FullHouse(Weak) -> 22
        | Flush(_) -> 
          match texture.Monoboard with
          | 4 -> 29
          | 5 -> 35
          | _ -> 20
        | Straight(Normal) -> 18
        | Straight(Weak) -> 19
        | ThreeOfKind -> 17
        | TwoPair -> 16
        | Pair(x) -> 
          let highKicker k = k = Ace || k = King || k = Queen || k = Jack
          match x with
          | Over -> 8
          | Top(k) when highKicker k -> 9
          | Top(_) -> 10
          | Second(k) when highKicker k -> 11
          | Second(_) -> 12
          | Third(_) -> 13
          | Fourth | Fifth -> 14
          | Under -> 15
        | TwoOvercards | Nothing -> 6
        )
      let column = 
        match texture.Monoboard, handValue with
        | 4, Flush(Nut) -> 4
        | 4, Flush(NotNut King) | 4, Flush(NotNut Queen) | 4, Flush(NotNut Jack) -> 2
        | 4, Flush(_) -> 0
        | 4, _ -> 4
        | _ -> 0
      (index, column)

    let monoMapping () =
      match handValue with
      | StraightFlush | FourOfKind -> (36, 0)
      | Flush(Board) -> (35, 0)
      | Flush(Nut) -> (35, 2)
      | Flush(NotNut King) | Flush(NotNut Queen) | Flush(NotNut Jack) -> (35, 5)
      | Flush(_) -> (35, 3)
      | _ -> (6, 0)

    let xlWorkSheet = xlWorkBook.Worksheets.[sheetName] :?> Worksheet
    
    let (index, column) =
      if texture.Monoboard = 5 then monoMapping ()
      else defaultMapping ()

    let indexString = index |> string
    let cellValues = getCellValues xlWorkSheet ("AE" + indexString) ("AK" + indexString)

    let (specialConditionsColumn, specialColumn) = 
      match texture.Monoboard with
      | 5 -> (None, 0)
      | 4 -> (Some 6, 5)
      | _ -> (Some 2, 3)
    let sc = Option.map (fun scc -> specialConditionsApply texture cellValues.[scc]) specialConditionsColumn |> defaultArg <| false

    parseOopOptionWithSpecialBoard cellValues.[column] sc cellValues.[specialColumn] cellValues.[1]

  let importFlopList sheetName (xlWorkBook : Workbook) =
    let xlWorkSheet = xlWorkBook.Worksheets.[sheetName] :?> Worksheet
    getCellValues xlWorkSheet "D2" "D150" 
    |> List.ofArray
    |> List.takeWhile (String.IsNullOrEmpty >> not)
    |> List.map (fun x -> x.Split(' ') |> List.ofArray |> List.map (fun c -> parseFace c.[0]))

  let importRange sheetName row (xlWorkBook : Workbook) =
    let xlWorkSheet = xlWorkBook.Worksheets.[sheetName] :?> Worksheet
    let rowString = row |> string
    (getCellValues xlWorkSheet ("A" + rowString) ("B" + rowString) ).[0]