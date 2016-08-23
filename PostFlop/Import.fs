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
open PostFlop.Parsing

module Import =
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

  let defaultOopOptions = { First = OopDonk.Check; Then = OopOnCBet.Fold; Special = []; Scenario = null; SpecialScenario = null }

  let orElse b a = match a with | Some _ -> a | None -> b()

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

  let rowIndex board =
    board 
    |> Seq.take 3 
    |> Seq.map (fun x -> x.Face) 
    |> Seq.map (fun x -> (faceValue x) - 2)
    |> Seq.sort
    |> Seq.mapi (fun i x -> 
      if i = 2 then x 
      else if i = 1 then [13-x..12] |> Seq.sum
      else [13-x..12] |> Seq.map (fun y -> [1..y] |> Seq.sum) |> Seq.sum)
    |> Seq.sum 

  let importOptions (xlWorkBook : Workbook) (hand: SuitedHand) (board: Board) limpedPot =
    let h = toHand hand
    let worksheetName = [h.Face1; h.Face2] |> List.sortByDescending faceValue |> List.map faceToChar |> String.Concat
    let xlWorkSheet = xlWorkBook.Worksheets.[worksheetName] :?> Worksheet
    let index = board |> rowIndex |> (+) 6 |> string
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

  let turnDonkOrFloatOopRow offset handValue s texture =
    let isTurnOvercard = isLastBoardCardOvercard s.Board
    let isTurnMiddlecard = isLastBoardCardSecondCard s.Board
    let isTurnAce = (Array.last s.Board).Face = Ace
    let handHasAce = s.Hand.Card1.Face = Ace || s.Hand.Card2.Face = Ace
    let overs = overcards s.Hand s.Board
    let highKicker k = k = Ace || k = King || k = Queen || k = Jack
    let highBoardPair = pairFace s.Board |> Option.filter highKicker |> Option.isSome
    let topPairedBoard = topPaired s.Board
    let isPairedBoard = isPaired s.Board
    let noDrawRow = 
      match handValue.Made with
      | Nothing -> 
        if handHasAce && isTurnOvercard then 10
        elif handHasAce then 9
        elif overs = 1 && isTurnOvercard then 13
        elif overs = 1 then 12
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
      | Straight(Weak) | Straight(OnBoard) -> 40
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
      match handValue.FD2, handValue.SD with 
       | Draw(_), _ when texture.Monoboard < 3 && flushDrawRow > 0 -> flushDrawRow
       | NoFD, OpenEnded when openEndedRow > 0 -> openEndedRow
       | NoFD, GutShot when gutShotRow > 0 -> gutShotRow 
       | _ -> noDrawRow
    (row + offset - 6) |> string

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
    let betSize = 100 * s.VillainBet / (s.Pot - s.VillainBet)
    let hasFlush = match handValue.Made with | Flush(_) | StraightFlush -> true | _ -> false
    let row = turnDonkOrFloatOopRow 6 handValue s texture
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
      | FullHouse(Weak) | FullHouse(OnBoard) -> 20
      | Flush(_) -> 18
      | Straight(Normal) -> 16
      | Straight(Weak) | Straight(OnBoard) -> 17
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
      let canonic = s.Trim().ToLowerInvariant()
      let parts = canonic.Split([|'/'|], 2)
      if parts.Length = 1 then
        match parts.[0] with
        | StartsWith "ai#" i -> match i with | Int x -> CallEQPlusXvsAI x | _ -> failwith "Failed parsing special rules (ai)"
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
        | StartsWith "bovso#" d -> match d with | DecimalPerc x -> BoardOvercard(Donk x, StackOff) | _ -> failwith "Failed parsing special rules (bovso)"
        | "a" -> BoardAce (OopDonk.AllIn, OopOnCBet.AllIn)
        | "aso" -> BoardAce(Donk 67m, StackOff)
        | "5" -> CheckCheck (Donk 75m, OopOnCBet.Call)
        | "7" -> CheckCheck (Donk 75m, StackOff)
        | "ov ch ch" -> CheckCheckAndBoardOvercard (Donk 75m, CallEQ 22)
        | "60" -> KHighOnPaired
        | "chrovso" -> BoardOvercard(OopDonk.Check, StackOffGay)
        | "1" -> NotUsed
        | _ -> failwith "Failed parsing special rules (1)"
      elif parts.Length = 2 then
        match parts.[0], parts.[1] with
        | "a", "f" -> BoardAce(Donk 67m, OopOnCBet.Fold)
        | StartsWith "bov#" d, Int c -> match d with | DecimalPerc x -> BoardOvercard(Donk x, CallEQ c) | _ -> failwith "Failed parsing special rules (bov)"
        | "chrov", Int c -> BoardOvercard(OopDonk.Check, RaiseGayCallEQ c)
        | "chrovb", Int c -> CheckRaiseOvercardBluff(RaiseCallEQ c)
        | StartsWith "xoxo#" d, Int c -> match d with | DecimalPerc x -> CheckCheck(Donk x, CallEQ c) | _ -> failwith "Failed parsing special rules (xoxo)"
        | _ -> failwith "Failed parsing special rules (2)"
      else failwith "Failed parsing special rules (3)"
    specialRules.Split ','
    |> Array.filter (fun x -> not(String.IsNullOrEmpty(x)))
    |> List.ofArray 
    |> List.map parseOopSpecialRule

  let rec parseOopOption (strategy: string) (specialRules: string) =
    let rec cbetParse s =
      let parseConditionalRaise x (s: string) =
        let parts = s.Split '/'
        let minStackRemaining =
          match parts.[0] with
          | StartsWith "$" v -> match v with | Int i -> i | _ -> 0
          | _ -> 0
        let minStackPotRatio =
          match parts.[0] with
          | StartsWith "&" v -> match v with | Decimal d -> d | _ -> 0m
          | _ -> 0m
        OopOnCBet.Raise { Size = x; MinStackRemaining = minStackRemaining; MinStackPotRatio = minStackPotRatio; On3Bet = cbetParse parts.[1] }
      match s with
      | StartsWith "r/" p3 -> 
        match p3 with 
        | "f" -> OopOnCBet.RaiseFold(2.75m)
        | "c" -> OopOnCBet.RaiseCall
        | Int i -> OopOnCBet.RaiseCallEQ i
        | _ -> failwith "Failed parsing Flop Oop OnCbet raise"
      | StartsWith "f1rr/" p3 -> cbetParse p3 |> FormulaRaise
      | StartsWith "rtfbft" p3 -> parseConditionalRaise 2.9m p3
      | StartsWith "rcombo" p3 -> parseConditionalRaise 2.2m p3
      | "f" -> OopOnCBet.Fold
      | "so" -> OopOnCBet.StackOff
      | "so+" -> OopOnCBet.StackOffFast
      | "c" -> OopOnCBet.Call
      | "ai" -> OopOnCBet.AllIn
      | Int i -> CallEQ i
      | _ -> failwith "Failed parsing Flop Oop OnCbet" 


    let canonic = strategy.Trim().ToLowerInvariant().Replace("!air", "")
    let scenarioParts = canonic.Split([|'*'|], 2)
    let scenario = if scenarioParts.Length > 1 then scenarioParts.[1] else null
    let specialScenarioParts = specialRules.Split([|'*'|], 2)
    let specialScenario = if specialScenarioParts.Length > 1 then specialScenarioParts.[1] else null
    let specialParts = scenarioParts.[0].Split([|'@'|], 2)
    if specialParts.Length = 2 then parseOopOption specialParts.[0] specialParts.[1]
    else
    let parts = specialParts.[0].Split([|'/'|], 2)
    if System.String.IsNullOrEmpty(strategy) then None
    else if parts.Length = 1 then
      match parts.[0] with 
      | "ai" -> Some { First = OopDonk.AllIn; Then = OopOnCBet.AllIn; Special = parseOopSpecialRules specialScenarioParts.[0]; Scenario = null; SpecialScenario = specialScenario }
      | "x" -> None
      | _ -> failwith "Failed parsing Flop Oop (1)"
    else if parts.Length = 2 then
      let donk = 
        match parts.[0] with 
        | "ch" -> OopDonk.Check
        | "rbs" -> OopDonk.RiverBetSizing
        | DecimalPerc n -> OopDonk.Donk n
        | _ -> failwith "Failed parsing Flop Oop Donk"
      let cbet = cbetParse parts.[1]
      Some { First = donk; Then = cbet; Special = parseOopSpecialRules specialScenarioParts.[0]; Scenario = scenario; SpecialScenario = specialScenario }
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
      | FullHouse(Weak) | FullHouse(OnBoard) -> 23
      | Flush(_) -> 21
      | Straight(Normal) -> 19
      | Straight(Weak) | Straight(OnBoard) -> 20
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

  let importOopRiver (xlWorkBook : Workbook) sheetName handValue texture s =
    let defaultMapping () =

      let turnCard = s.Board.[3]
      let isRiverOvercard = isLastBoardCardOvercard s.Board
      let isRiverUndercard = isLastBoardCardUndercard s.Board
      let isRiverMiddlecard = not(isRiverOvercard) && s.Board |> Array.except [turnCard] |> isLastBoardCardOvercard

      let turn = s.Board |> Array.take 4
      let isTurnOvercard = isLastBoardCardOvercard turn

      let isPairedBoard = isPaired s.Board

      let row = 
        (match handValue with
        | StraightFlush | FourOfKind ->
          match texture.Monoboard with
          | 4 -> 49
          | 5 -> 55
          | _ -> 23
        | FullHouse(Normal) -> 21
        | FullHouse(Weak) | FullHouse(OnBoard) -> 22
        | Flush(_) -> 
          match texture.Monoboard with
          | 4 -> 48
          | 5 -> 54
          | _ -> 20
        | Straight(Normal) -> 18
        | Straight(Weak) | Straight(OnBoard) -> 19
        | ThreeOfKind -> 17
        | TwoPair -> 
          let pairIndeces = pairIndeces s.Hand s.Board
          let topPair = pairIndeces |> Seq.head = 1
          let topTwoPairs = pairIndeces |> Seq.take 2 = seq [1; 2]

          if topTwoPairs && not isPairedBoard then 40
          elif topPair && not isPairedBoard then 41
          elif topPair && bottomPaired s.Board then 42
          else 16
        | Pair(x) -> 
          let highKicker k = k = Ace || k = King || k = Queen || k = Jack
          match x with
          | Over -> 8
          | Top(k) when highKicker k -> 9
          | Top(_) -> 10
          | Second(k) when highKicker k -> 11
          | Second(_) -> 12
          | Third(_) -> 
            if isTurnOvercard && isRiverOvercard then 28
            elif isTurnOvercard && isRiverMiddlecard then 29
            elif isRiverOvercard then 30
            elif isTurnOvercard && isRiverUndercard then 31
            else 13
          | Fourth ->
            if isTurnOvercard && isRiverOvercard then 32
            elif isTurnOvercard && isRiverMiddlecard then 33
            elif isRiverOvercard then 34
            elif isTurnOvercard && isRiverUndercard then 35
            else 14
          | Fifth ->
            if isTurnOvercard && isRiverOvercard then 36
            elif isTurnOvercard && isRiverMiddlecard then 37
            elif isRiverOvercard then 38
            elif isTurnOvercard && isRiverUndercard then 39
            else 14
          | Under -> 15
        | TwoOvercards | Nothing -> 
          let riverDoesNotPair = s.Board |> Array.filter (fun x -> x.Face = s.Board.[4].Face) |> Array.length = 1
          let isTurnOrRiverAce = s.Board |> Array.skip 3 |> Array.exists (fun x -> x.Face = Ace)
          let turnDoesNotPair = s.Board |> Array.filter (fun x -> x.Face = turnCard.Face) |> Array.length = 1

          if isTurnOvercard && isRiverOvercard && not(isTurnOrRiverAce) then 24
          elif isTurnOvercard && isRiverMiddlecard && not(isTurnOrRiverAce) then 25
          elif isRiverOvercard && turnDoesNotPair && not(isTurnOrRiverAce) then 26
          elif isTurnOvercard && isRiverUndercard && riverDoesNotPair && not(isTurnOrRiverAce) then 27
          else 6
        )
      let column = 
        match texture.Monoboard, handValue with
        | 4, Flush(Nut) -> 4
        | 4, Flush(NotNut King) | 4, Flush(NotNut Queen) | 4, Flush(NotNut Jack) -> 2
        | 4, Flush(_) -> 0
        | 4, _ -> 5
        | 3, Flush(_) -> 0
        | 3, _ when isLastBoardCardFlushy s.Board -> 1
        | _ -> 0
      (row, column)

    let monoMapping () =
      match handValue with
      | StraightFlush | FourOfKind -> (55, 0)
      | Flush(Board) -> (54, 0)
      | Flush(Nut) -> (54, 2)
      | Flush(NotNut King) | Flush(NotNut Queen) | Flush(NotNut Jack) -> (54, 5)
      | Flush(_) -> (54, 3)
      | _ -> (6, 0)

    let xlWorkSheet = xlWorkBook.Worksheets.[sheetName] :?> Worksheet
    
    let (index, column) =
      if texture.Monoboard = 5 then monoMapping ()
      else defaultMapping ()

    let indexString = index |> string
    let cellValues = getCellValues xlWorkSheet ("AE" + indexString) ("AL" + indexString)

    let (specialConditionsColumn, specialColumn, specialRulesColumn) = 
      match texture.Monoboard with
      | 5 -> (None, 0, None)
      | 4 -> (Some 7, 6, None)
      | _ -> (Some 3, 4, Some 2)
    let sc = Option.map (fun scc -> specialConditionsApply texture cellValues.[scc]) specialConditionsColumn |> defaultArg <| false
    let sr = defaultArg (Option.map (fun src -> cellValues.[src]) specialRulesColumn) ""

    parseOopOptionWithSpecialBoard cellValues.[column] sc cellValues.[specialColumn] sr

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

  let importRiverBetSizes (xlWorkBook : Workbook) = 
    let parsePercentage s =
      match s with
      | Decimal d -> d * 100m |> int
      | _ -> failwith ("Failed parsing river bet size " + s)
    let xlWorkSheet = xlWorkBook.Worksheets.["r8 & r9  riv bet sizings"] :?> Worksheet
    [3..10]
    |> Seq.map (fun row -> getCellValues xlWorkSheet ("A" + row.ToString()) ("E" + row.ToString()))
    |> Seq.takeWhile (fun vs -> not(String.IsNullOrEmpty(vs.[0])))
    |> Seq.map (fun vs ->  
       let potParts = vs.[0].Split([|'-'; '+'|], StringSplitOptions.RemoveEmptyEntries)
       { MinPotSize = Int32.Parse(potParts.[0])
         MaxPotSize = if potParts.Length > 1 then Int32.Parse(potParts.[1]) else 1000
         MinAllInPercentage = parsePercentage vs.[1]
         MaxAllInPercentage = parsePercentage vs.[2]
         BetSize = parsePercentage vs.[3]
         MinChipsLeft = Int32.Parse(vs.[4])
       })
    |> List.ofSeq


  let importFloatFlopOptions (xlWorkBook : Workbook) s =
    let xlWorkSheet = xlWorkBook.Worksheets.["float OOP"] :?> Worksheet
    let index = s.Board |> rowIndex |> (+) 5 |> string
    let cellValues = getCellValues xlWorkSheet ("F" + index) ("G" + index)
    
    let villainBetSize = relativeBet s
    let rangesToCompare = seq {
      if villainBetSize = 0 || (villainBetSize >= 26 && villainBetSize <= 57) then yield (0, BluffFloat)
      if villainBetSize = 0 || villainBetSize >= 26 then yield (1, ValueFloat)
    }
    
    rangesToCompare
    |> Seq.map (fun (i, m) -> (Ranges.parseRanges cellValues.[i], m))
    |> Seq.filter (fun (r, _) -> toHand s.Hand |> Ranges.isHandInRanges r)
    |> Seq.map snd
    |> Seq.tryHead
    |> Option.map (fun m -> ({ defaultOopOptions with Then = OopOnCBet.Call }, Some(Float m)))

  let importFloatTurnOptions (xlWorkBook : Workbook) value texture s h =
    let xlWorkSheet = xlWorkBook.Worksheets.["float OOP"] :?> Worksheet
    let villainBet = relativeBet s
    let villainBetHigh = villainBet >= 60
    let row = turnDonkOrFloatOopRow 5 value s texture
    let yellowColumn = 
      match texture.Monoboard, value.FD with
      | 3, NoFD -> if texture.Streety then "U" else "T"
      | 3, Draw(Nut) -> if villainBetHigh then "X" else "W"
      | 3, Draw(NotNut k) when k = King || k = Queen -> if villainBetHigh then "Z" else "Y"
      | 3, Draw(NotNut k) when k = Jack || k = Ten -> if villainBetHigh then "AB" else "AA"
      | 3, Draw(NotNut x) when x = Nine || x = Eight || x = Seven -> if villainBetHigh then "AD" else "AC"
      | 3, Draw(NotNut x) -> if villainBetHigh then "AF" else "AE"
      | 3, Draw(Board) -> failwith "Board draw impossible on 3 monoboard"
      | _ ->
        if texture.Streety then "P" 
        elif texture.DoublePaired then "Q"
        elif texture.ThreeOfKind then "R"
        elif villainBetHigh then "O"
        else "N"
    let greenColumn = 
      match texture.Monoboard, value.FD with
      | 3, NoFD -> "AJ"
      | 3, Draw(Nut) -> if villainBetHigh then "AM" else "AL"
      | 3, Draw(NotNut k) when k = King || k = Queen -> if villainBetHigh then "AO" else "AN"
      | 3, Draw(NotNut k) when k = Jack || k = Ten -> if villainBetHigh then "AQ" else "AP"
      | 3, Draw(NotNut x) when x = Nine || x = Eight || x = Seven -> if villainBetHigh then "AS" else "AR"
      | 3, Draw(NotNut x) -> if villainBetHigh then "AU" else "AT"
      | 3, Draw(Board) -> failwith "Board draw impossible on 3 monoboard"
      | _ -> "AH"

    let flopFloatMotivation =
      h 
      |> List.filter (fun hi -> hi.Street = Flop && hi.Motivation.IsSome) 
      |> List.tryHead
      |> Option.bind (fun f -> f.Motivation)

    let column = 
      if villainBet > 0 && villainBet < 26 then None
      else match flopFloatMotivation with
           | Some(Float BluffFloat) -> Some yellowColumn
           | Some(Float ValueFloat) -> Some greenColumn
           | _ -> None

    let motivation (cell: string) = 
      if cell.Contains("&") then Some(Float(WithContinuation "62.5%/f"))
      elif cell.Contains("!air") then Some(Float(WithContinuation "75%/so"))
      else flopFloatMotivation

    column
    |> Option.map (fun col -> getCellValue xlWorkSheet (col + row))
    |> Option.bind (fun cell -> 
      match parseOopOption cell "" with
      | Some o -> Some (o, motivation cell)
      | None -> None)

  let importFloatRiverOptions (xlWorkBook : Workbook) value texture s h =
    let xlWorkSheet = xlWorkBook.Worksheets.["float OOP"] :?> Worksheet
    let villainBet = relativeBet s
    let handHasAce = s.Hand.Card1.Face = Ace || s.Hand.Card2.Face = Ace
    let isRiverAce = (Array.last s.Board).Face = Ace
    let turn = s.Board |> Array.take 4
    let turnCard = s.Board.[3]
    let isTurnAce = (Array.last turn).Face = Ace
    let aceOnlyOnFlop = s.Board |> Array.exists (fun c -> c.Face = Ace) && not isTurnAce && not isRiverAce
    let isTurnOvercard = isLastBoardCardOvercard turn
    let isRiverOvercard = isLastBoardCardOvercard s.Board
    let areTurnAndRiverOverFlop = isTurnOvercard && isLastBoardCardOvercard (s.Board |> Array.except [turnCard])
    let isRiverMiddlecard = isLastBoardCardSecondCard s.Board
    let isRiverUndercard = isLastBoardCardUndercard s.Board
    let riverDoesNotPair = s.Board |> Array.filter (fun x -> x.Face = s.Board.[4].Face) |> Array.length = 1
    let isTurnOrRiverAce = isTurnAce || isRiverAce
    let turnDoesNotPair = s.Board |> Array.filter (fun x -> x.Face = turnCard.Face) |> Array.length = 1
    let highKicker k = k = Ace || k = King || k = Queen || k = Jack
    let topPairedBoard = topPaired s.Board
    let isPairedBoard = isPaired s.Board
    let topPairedBoardOnRiver = topPairedBoard && not isPairedBoard
    let row = 
      match value with
      | Nothing | TwoOvercards -> 
        if handHasAce then 6
        elif isTurnOvercard && isRiverOvercard && not isTurnOrRiverAce then 8
        elif isTurnOvercard && isRiverMiddlecard && not isTurnOrRiverAce then 9
        elif isRiverOvercard && turnDoesNotPair && not isTurnOrRiverAce then 10
        elif isTurnOvercard && isRiverUndercard && riverDoesNotPair && not isTurnOrRiverAce then 11
        elif aceOnlyOnFlop then 7
        else 5
      | Pair(Over) -> 12
      | Pair(Top k) when highKicker k -> 13
      | Pair(Top _) -> 14      
      | Pair(Second k) when isPairedBoard && isRiverOvercard -> 24
      | Pair(Second k) when isPairedBoard && isTurnOvercard -> 25
      | Pair(Second k) when highKicker k && topPairedBoardOnRiver -> 19
      | Pair(Second k) when topPairedBoardOnRiver -> 20
      | Pair(Second k) when highKicker k && topPairedBoard -> 17
      | Pair(Second k) when topPairedBoard -> 18
      | Pair(Second k) when isRiverOvercard -> 21
      | Pair(Second k) when isTurnOvercard -> 22
      | Pair(Second k) when k > turnCard.Face && k > s.Board.[4].Face -> 23
      | Pair(Second k) when highKicker k -> 15
      | Pair(Second _) -> 16
      | Pair(Third _) when areTurnAndRiverOverFlop -> 27
      | Pair(Third _) -> 26
      | Pair(Fourth _) when areTurnAndRiverOverFlop -> 29
      | Pair(Fourth _) -> 28
      | Pair(Fifth _) when areTurnAndRiverOverFlop -> 30
      | Pair(Fifth _) -> 31
      | Pair(Under) -> 32
      | TwoPair -> 
        let pairIndeces = pairIndeces s.Hand s.Board
        let topPair = pairIndeces |> Seq.head = 1
        let topTwoPairs = pairIndeces |> Seq.take 2 = seq [1; 2]

        if topTwoPairs && not isPairedBoard then 33
        elif topPair && not isPairedBoard then 34
        elif topPair && bottomPaired s.Board then 36
        else 35
      | ThreeOfKind -> 37
      | Straight(Normal) -> 38
      | Straight(Weak) -> 39
      | Straight(OnBoard) -> 44
      | Flush(x) -> 
        if texture.Monoboard = 4 then
          match x with
          | Nut -> 51
          | NotNut(King) -> 52
          | NotNut(Queen) -> 53
          | NotNut(Jack) -> 54
          | NotNut(Ten) | NotNut(Nine) -> 55
          | NotNut(Eight) | NotNut(Seven) -> 56
          | NotNut(Six) | NotNut(Five) -> 57
          | _ -> 58
        else 40
      | FullHouse(Normal) -> 41
      | FullHouse(Weak) -> 42
      | FullHouse(OnBoard) -> 45
      | FourOfKind ->
        if texture.FourOfKind then 
          let kicker = maxFace [s.Hand.Card1; s.Hand.Card2]
          let isNuts = kicker = Ace || (kicker = Ace && s.Board.[0].Face = King)
          if isNuts then 46
          elif kicker = King || kicker = Queen || kicker = Jack then 47 else 48
        else 43
      | StraightFlush -> 43
      |> string
    let column = 
      match texture.Monoboard, value with
      | 4, Flush(_) -> "AX"
      | 4, _ -> if texture.Streety then "BE" else "BD"
      | _ ->
        if texture.Streety then "AZ" 
        elif texture.DoublePaired then "BA"
        elif texture.ThreeOfKind then "BB"
        else "AX"
    let floatedBefore = h |> List.exists (fun hi -> match hi.Motivation with | Some(Float _) -> true | _ -> false)
    let column = 
      if (villainBet > 0 && villainBet < 26) || not floatedBefore then None
      else Some column
    let continuation =  h |> List.tryLast |> Option.bind (fun hi -> match hi.Motivation with | Some(Float(WithContinuation x)) -> Some (x, "") | _ -> None)
    
    continuation 
    |> orElse (fun () -> column |> Option.map (fun col -> (getCellValue xlWorkSheet (col + row), if col = "AX" then getCellValue xlWorkSheet ("AY" + row) else "")))
    |> Option.bind (fun (c1, c2) -> parseOopOption c1 c2)