namespace PostFlop

open Microsoft.FSharp.Core
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

  let defaultIpOptions = { CbetFactor = Never; CheckRaise = OnCheckRaise.Call; Donk = OnDonk.Fold; DonkRaise = OnDonkRaise.Undefined  }
  let defaultOopOptions = { First = OopDonk.Check; Then = OopOnCBet.Fold; Special = []; Scenario = null; SpecialScenario = null }

  let excelColumns = ["A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";"N";"O";"P";"Q";"R";"S";"T";"U";"V";"W";"X";"Y";"Z";"AA";"AB";"AC";"AD";"AE";"AF";"AG";"AH";"AI";"AJ";"AK";"AL";"AM";"AN";"AO";"AP";"AQ";"AR";"AS";"AT";"AU";"AV";"AW";"AX";"AY";"AZ";"BA";"BB";"BC";"BD"]

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

  let parseCheckRaise (s: string) =
    match s.ToLowerInvariant() with
    | "so" -> OnCheckRaise.StackOff
    | "c" -> OnCheckRaise.Call
    | "ai" -> OnCheckRaise.AllIn
    | "f" -> OnCheckRaise.Fold
    | Int i -> OnCheckRaise.CallEQ i
    | _ -> OnCheckRaise.Undefined

  let parseCheckRaise2 (g: string) (h: string) =
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
      CheckRaise = parseCheckRaise2 cellValues.[1] cellValues.[2]
      Donk = parseDonk cellValues.[3] cellValues.[4]
      DonkFlashDraw = parseDonkFD cellValues.[5]
      TurnFVCbetCards = cellValues.[6].Replace(" ", "")
      TurnFVCbetFactor = 
        match parseDecimal cellValues.[7] with
        | Some n -> OrAllIn { DefaultCBetOr with Factor = n; IfPreStackLessThan = if limpedPot then 8 else 14 }
        | None -> Never 
      TurnCheckRaise = parseCheckRaise2 cellValues.[8] "100"
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
    | "call/raise" -> (OnDonk.CallRaisePet, OnDonkRaise.StackOff)
    | Int i -> (OnDonk.CallEQ i, OnDonkRaise.Undefined)
    | _ -> (OnDonk.Undefined, OnDonkRaise.Undefined)

  let specialConditionsApply sc (v: string) = 
    [(sc.Streety, "1"); (sc.DoublePaired, "3")]
    |> List.filter fst
    |> List.map snd
    |> List.exists (fun x -> v.Contains(x))

  let rec parseTurnDonk (strategy: string) =
    let rec parseFormulaRaiseDonk = function
      | StartsWith "\"" s -> parseTurnDonk s |> fst
      | _ -> OnDonk.AllIn

    let canonic = strategy.Trim().ToLowerInvariant()
    let parts = canonic.Split([|'/'|], 2)
    if System.String.IsNullOrEmpty(strategy) then (OnDonk.Undefined, OnDonkRaise.Undefined)
    else if parts.Length = 1 then
      match parts.[0] with 
      | "c" | "call" -> (OnDonk.Call, OnDonkRaise.Undefined)
      | StartsWith "call^" s -> 
        match s with 
        | Decimal x -> (OnDonk.RaiseConditional { Size = 2.2m; MinStackPotRatio = x }, OnDonkRaise.StackOff)
        | _ -> failwith ("Failed parsing Turn Donk (c^)" + s)
      | "f" -> (OnDonk.Fold, OnDonkRaise.Undefined)
      | "ai" -> (OnDonk.AllIn, OnDonkRaise.Undefined)
      | "so" -> (OnDonk.ForValueStackOff, OnDonkRaise.StackOff)
      | Int n -> (OnDonk.CallEQ n, OnDonkRaise.Undefined)
      | _ -> failwith ("Failed parsing Turn Donk (1)" + strategy)      
    elif parts.Length = 2 then
      let donk = 
        match parts.[0] with 
        | "rtsdb" -> OnDonk.RaisePreDonkX 110
        | "rtbdb" -> OnDonk.RaiseX 260
        | "rtg" -> OnDonk.RaiseGay
        | "rtfb" -> OnDonk.RaiseX 220
        | "f1rr" -> OnDonk.FormulaRaise
        | StartsWith "rtv" s -> parseFormulaRaiseDonk s |> OnDonk.RaiseThinValue
        | StartsWith "rmodx" s -> match s with | Decimal x -> OnDonk.RaiseX (x * 100m |> int) | _ -> failwith ("Failed parsing Turn Donk (5)" + strategy)
        | _ -> failwith ("Failed parsing Turn Donk (2)" + strategy)
      let raise =
        match parts.[1] with 
        | "so" | "sot" -> OnDonkRaise.StackOff
        | "c" -> OnDonkRaise.Call
        | Int n -> OnDonkRaise.CallEQ n
        | _ -> failwith ("Failed parsing TurnDonk (3)"  + strategy)
      (donk, raise)
    else failwith ("Failed parsing TurnDonk (4)" + strategy)

  let turnDonkOrFloatOopRow offset handValue s texture =
    let isTurnOvercard = isLastBoardCardOvercard s.Board
    let isTurnMiddlecard = isLastBoardCardSecondCard s.Board
    let isTurnAce = (Array.last s.Board).Face = Ace
    let isFlopAce = s.Board |> Array.take 3 |> Array.exists (fun c -> c.Face = Ace)
    let handHasAce = s.Hand.Card1.Face = Ace || s.Hand.Card2.Face = Ace
    let overs = overcards s.Hand s.Board
    let highKicker k = k = Ace || k = King || k = Queen || k = Jack
    let highBoardPair = pairFace s.Board |> Option.filter highKicker |> Option.isSome
    let topPairedBoard = topPaired s.Board
    let isPairedBoard = isPaired s.Board
    let noDrawRow = 
      match handValue.Made with
      | Nothing -> 
        if handHasAce && isTurnOvercard then 11
        elif handHasAce then 9
        elif overs = 1 && isTurnOvercard then 14
        elif overs = 1 then 13
        elif isTurnAce then 8 
        elif isTurnOvercard then 7 
        elif isFlopAce then 10
        elif isTurnMiddlecard then 12
        else 6
      | TwoOvercards -> if isTurnOvercard then 16 else 15
      | Pair(x) -> 
        match x with
        | Over -> 18
        | Top(k) when highKicker k -> 19
        | Top(_) when highBoardPair -> 21
        | Top(_) -> 20
        | Second(x) when topPairedBoard -> if highKicker x then 22 else 23
        | Second(x) when isPairedBoard -> if highKicker x then 25 else 27
        | Second(x) when isTurnOvercard -> if highKicker x then 28 else 29
        | Second(x) -> if highKicker x then 24 else 26
        | Third when isPairedBoard -> 31
        | Third when isTurnAce -> 33
        | Third when isTurnOvercard -> 32
        | Third -> 30
        | Fourth when isTurnAce -> 35
        | Fourth -> 34
        | Fifth -> failwith "Fifth pair impossible on turn"
        | Under -> 36
      | TwoPair when isPairedBoard -> 38
      | TwoPair -> 37
      | ThreeOfKind -> 39
      | Straight(Normal) -> 40
      | Straight(Weak) | Straight(OnBoard) -> 41
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
        else 42
      | FullHouse(x) ->
        match tripsFace s.Board with
        | Some(k) -> 
          let kicker = concat s.Hand s.Board |> anyPairFace |> Option.get
          if highKicker kicker then 45 else 44
        | None -> if x = Normal then 43 else 46
      | FourOfKind -> 
        if texture.FourOfKind then 
          let kicker = maxFace [s.Hand.Card1; s.Hand.Card2]
          let isNuts = kicker = Ace || (kicker = Ace && s.Board.[0].Face = King)
          if isNuts then 103
          elif kicker = King || kicker = Queen || kicker = Jack then 104 else 105
        else 47
      | StraightFlush -> 47
    let gutShotRow = 
      match handValue.Made with
      | Nothing -> 
        if overs = 1 && isTurnOvercard then 53
        elif overs = 1 then 52
        elif isTurnAce then 51
        elif isTurnOvercard then 50
        else 49
      | TwoOvercards -> if isTurnOvercard then 55 else 54
      | Pair(x) -> 
        match x with
        | Top(_) -> 56
        | Second(x) when topPairedBoard -> 59
        | Second(x) when isPairedBoard -> 60
        | Second(x) when isTurnOvercard -> 58
        | Second(x) -> 57
        | Third when isTurnOvercard -> 62
        | Third -> 61
        | Fourth when isTurnAce -> 64
        | Fourth when isTurnOvercard -> 65
        | Fourth -> 63
        | _ -> 0
      | _ -> 0
    let openEndedRow = 
      match handValue.Made with
      | Nothing -> 
        if overs = 1 && isTurnOvercard then 71
        elif overs = 1 then 70
        elif isTurnAce then 68
        elif isTurnOvercard then 69
        else 67
      | TwoOvercards -> if isTurnOvercard then 73 else 72
      | Pair(x) -> 
        match x with
        | Top(_) -> 74
        | Second(x) when topPairedBoard -> 77
        | Second(x) when isPairedBoard -> 78
        | Second(x) when isTurnOvercard -> 76
        | Second(x) -> 75
        | Third when isTurnOvercard -> 80
        | Third -> 79
        | Fourth when isTurnAce -> 82
        | Fourth -> 81
        | Under -> 83
        | _ -> 0
      | _ -> 0
    let flushDrawRow = 
      match handValue.Made with
      | Nothing -> 
        match handValue.SD with
        | OpenEnded -> 92
        | GutShot when isTurnOvercard -> 94
        | GutShot -> 93
        | NoSD ->
          if overs = 1 && isTurnOvercard then 88
          elif overs = 1 then 87
          elif isTurnOvercard then 86
          else 85
      | TwoOvercards -> if isTurnOvercard then 90 else 89
      | Pair(x) -> 
        match x with
        | Top(_) -> 91
        | Second(x) when topPairedBoard -> 97
        | Second(x) when isPairedBoard -> 98
        | Second(x) when isTurnOvercard -> 96
        | Second(x) -> 95
        | Third when isTurnOvercard -> 100
        | Third -> 99
        | Fourth when isTurnOvercard -> 102
        | Fourth -> 101
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
    let villain3betPre = h |> List.tryHead |> Option.filter (fun x -> x.VsVillainBet >= s.BB * 4) |> Option.isSome
    let historyFlop = h |> List.filter (fun x -> x.Street = Flop)
    let sheetName = 
      match historyFlop with
      | [{ Action = Action.Check }]
      | [{ Action = Action.RaiseToAmount(_); VsVillainBet = 0 }] when villain3betPre 
        -> "vill xr F + dbT or dbF-c + dbT"
      | [{ Action = Action.Check }] 
        -> "xx flop + vill bet turn"
      | [{ Action = Action.RaiseToAmount(_); VsVillainBet = 0 }] 
      | [{ Action = Action.Call }] 
        -> "vill xc F + dbT or dbF + dbT"
      | _ when historyFlop |> List.exists (fun x -> x.VsVillainBet > 0) 
        -> "vill xr F + dbT or dbF-c + dbT"
      | _ -> failwith "Could not pick turn donkbet sheet"

    let xlWorkSheet = xlWorkBook.Worksheets.[sheetName] :?> Worksheet
    let betSize = relativeDonkSize s h
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
    let scenarioParts = cellValue.Split([|'*'|], 2)
    let scenario = if scenarioParts.Length > 1 then scenarioParts.[1] else null
    let (t1, t2) = parseTurnDonk scenarioParts.[0]
    let motivation = if String.IsNullOrEmpty scenario then None else Some(Scenario(scenario))
    t1, t2, motivation, "HandStrength -> " + sheetName + " -> " + column + row

  let parseRiverCbet (i: string) =
    match i.ToLowerInvariant() with
    | "stack off" -> (OrAllIn { DefaultCBetOr with Factor = 62.5m; IfStackFactorLessThan = Some(4m/3m) }, OnCheckRaise.StackOff)
    | "check" -> (Never, OnCheckRaise.Undefined)
    | Decimal d -> (Always(d), OnCheckRaise.CallEQ 11)
    | _ -> failwith ("Failed parsing River on check" + i)

  let parseOopDonk = function
    | "ch" -> OopDonk.Check
    | "rbs" -> OopDonk.RiverBetSizing
    | "rbv" -> OopDonk.RiverBetValue
    | "rbtv" -> OopDonk.RiverBetThinValue
    | DecimalPerc n -> OopDonk.Donk n
    | v -> failwith ("Failed parsing Flop Oop Donk " + v)

  let rec parseOopCbet s =
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
      OopOnCBet.RaiseConditional { Size = x; MinStackRemaining = minStackRemaining; MinStackPotRatio = minStackPotRatio; On3Bet = parseOopCbet parts.[1] }

    let parseFormulaRaise = function
      | StartsWith "/" s -> { Or = OopOnCBet.AllIn; On3Bet = parseOopCbet s }
      | StartsWith "\"" s ->
        match s with
          | SplittedTuple '/' (a, b) -> { Or = parseOopCbet a; On3Bet = parseOopCbet b }
          | _ -> failwith ("Failed to parseFormulaRaise " + s)
      | v -> failwith ("Failed to parseFormulaRaise2 " + v)

    match s with
    | StartsWith "r/" p3 -> 
      match p3 with 
      | "f" -> OopOnCBet.Raise(2.75m, OopOnCBet.Fold)
      | "c" -> OopOnCBet.Raise(2.75m, OopOnCBet.Call)
      | Int i -> OopOnCBet.Raise(2.75m, OopOnCBet.CallEQ i)
      | _ -> failwith ("Failed parsing Flop Oop OnCbet raise" + s)
    | StartsWith "f1rr" p3 -> parseFormulaRaise p3 |> FormulaRaise
    | StartsWith "rtv" p3 -> parseFormulaRaise p3 |> RaiseThinValue
    | StartsWith "rtfbft" p3 -> parseConditionalRaise 2.9m p3
    | StartsWith "rcombo" p3 -> parseConditionalRaise 2.2m p3
    | StartsWith "rtbdb/" p3 -> OopOnCBet.Raise(2.6m, parseOopCbet p3)
    | StartsWith "rtfb/" p3 -> OopOnCBet.Raise(2.2m, parseOopCbet p3)
    | StartsWith "rmodx" s -> 
      let parts = s.Split('/')
      match parts.[0], parts.[1] with 
      | Decimal x, p3 -> OopOnCBet.Raise(x, parseOopCbet p3)
      | _ -> failwith ("Failed parsing Flop Oop OnCbet rmodx" + s)
    | "f" -> OopOnCBet.Fold
    | "so" | "sot" -> OopOnCBet.StackOff
    | "so+" -> OopOnCBet.StackOffFast
    | "c" -> OopOnCBet.Call
    | "ai" -> OopOnCBet.AllIn
    | SplittedTuple '"' (Int a, Int b) -> CallEQIfRaised(a,b)
    | Int i -> CallEQ i
    | _ -> failwith ("Failed parsing Flop Oop OnCbet" + s)

  let parseOopSpecialRules (specialRules: string) = 
    let parseOopSpecialRule (s: string) =
      let canonic = s.Trim().ToLowerInvariant()
      let parts = canonic.Split([|'/'|], 2)
      if parts.Length = 1 then
        match parts.[0] with
        | StartsWith "ai#" i -> match i with | Int x -> CallEQPlusXvsAI x | _ -> failwith ("Failed parsing special rules (ai)" + s)
        | "bp gs" -> PairedBoard (OopDonk.Check, CallEQ 14)
        | "bp fd" -> PairedBoard (OopDonk.Check, CallEQ 22)
        | "22" -> PairedBoard (Donk 50m, CallEQ 20)
        | "tpp" -> PairedBoard (OopDonk.AllIn, OopOnCBet.AllIn)
        | "6" -> BoardOvercard(OopDonk.Check, OopOnCBet.Call)
        | "ov" -> BoardOvercard(OopDonk.AllIn, OopOnCBet.AllIn)
        | "ov ai" -> BoardOvercard(OopDonk.Check, OopOnCBet.AllIn)
        | "ovso" -> BoardOvercard(Donk 67m, StackOff)
        | "61" -> BoardOvercard(Donk 60m, CallEQ 25)
        | "4" -> BoardOvercard(OopDonk.Check, StackOff)
        | "44" -> BoardOvercard(Donk 62.5m, CallEQ 20)
        | StartsWith "bovso#" d -> match d with | DecimalPerc x -> BoardOvercard(Donk x, StackOff) | _ -> failwith ("Failed parsing special rules (bovso)" + s)
        | "a" -> BoardAce (OopDonk.AllIn, OopOnCBet.AllIn)
        | "aso" -> BoardAce(Donk 67m, StackOff)
        | "5" -> CheckCheck (Donk 75m, OopOnCBet.Call)
        | "7" -> CheckCheck (Donk 75m, StackOff)
        | "ov ch ch" -> CheckCheckAndBoardOvercard (Donk 75m, CallEQ 22)
        | "60" -> KHighOnPaired
        | "chrovso" -> BoardOvercard(OopDonk.Check, StackOffGay)
        | "1" -> NotUsed
        | _ -> failwith ("Failed parsing special rules (1)" + s)
      elif parts.Length = 2 then
        match parts.[0], parts.[1] with
        | "a", "f" -> BoardAce(Donk 67m, OopOnCBet.Fold)
        | StartsWith "bov#" fs, ts -> BoardOvercard(parseOopDonk fs, parseOopCbet ts) 
        | "chrov", Int c -> BoardOvercard(OopDonk.Check, RaiseGayCallEQ c)
        | "chrovb", Int c -> CheckRaiseOvercardBluff(Raise(2.75m, OopOnCBet.CallEQ c))
        | StartsWith "xoxo#" fs, ts -> CheckCheck(parseOopDonk fs, parseOopCbet ts) 
        | _ -> failwith ("Failed parsing special rules (2)" + s)
      else failwith ("Failed parsing special rules (3)" + s)
    specialRules.Split ','
    |> Array.filter (fun x -> not(String.IsNullOrEmpty(x)))
    |> List.ofArray 
    |> List.map parseOopSpecialRule

  let rec parseOopOption (strategy: string) (specialRules: string) =
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
      | "ai" -> Some { First = OopDonk.AllIn; Then = OopOnCBet.AllIn; Special = parseOopSpecialRules specialScenarioParts.[0]; Scenario = scenario; SpecialScenario = specialScenario }
      | "ch" | "f" -> Some { First = OopDonk.Check; Then = OopOnCBet.Fold; Special = parseOopSpecialRules specialScenarioParts.[0]; Scenario = scenario; SpecialScenario = specialScenario }
      | Int i -> Some { First = OopDonk.Check; Then = OopOnCBet.CallEQ i; Special = parseOopSpecialRules specialScenarioParts.[0]; Scenario = scenario; SpecialScenario = specialScenario }
      | "x" -> None
      | _ -> failwith ("Failed parsing Flop Oop (1)" + strategy)
    else if parts.Length = 2 then
      let donk = parseOopDonk parts.[0]
      let cbet = parseOopCbet parts.[1]
      Some { First = donk; Then = cbet; Special = parseOopSpecialRules specialScenarioParts.[0]; Scenario = scenario; SpecialScenario = specialScenario }
    else failwith "Failed parsing Flop Oop (2)"

  let parseOopOptionWithSpecialBoard (strategy: string) isSpecialBoard (specialBoardStrategy:string) (specialRules: string) =
    if isSpecialBoard then
      match parseOopOption specialBoardStrategy specialRules with
      | None -> parseOopOption strategy specialRules
      | x -> x
    else parseOopOption strategy specialRules

  let streetPattern = function
    | [{Action = Action.Check}] -> "x/x"
    | [{Action = Action.RaiseToAmount(_); VsVillainBet = 0}] -> "x/c"
    | [{Action = Action.RaiseToAmount(_); VsVillainBet = b}] when b > 0 -> "db/c"
    | [{VsVillainBet = b}] when b > 0 -> "db"
    | [{Action = Action.RaiseToAmount(_); VsVillainBet = 0}; {Action = Action.RaiseToAmount(_)}] -> "3bet"
    | [{Action = Action.RaiseToAmount(_); VsVillainBet = 0}; _] -> "x/r"
    | [{Action = Action.RaiseToAmount(_); VsVillainBet = b}; _] when b > 0 -> "3bet"
    | fst::snd::_ -> "3bet"
    | _ -> failwith "Failed to recognize street history pattern"

  let historyPattern s h =
    let flopTurnPattern =
      [Flop; Turn]
      |> List.map (fun s-> h |> List.filter (fun x -> x.Street = s) |> streetPattern)
      |> (fun x -> System.String.Join("+", x))
    if flopTurnPattern.Contains("3bet") 
    then "3bet" 
    else flopTurnPattern + "+" + if donkedOnThisStreet s h then "db" else "x"

  let importRiverPatterns (xlWorkBook : Workbook) = 
    [for sheetName in ["river - villain check"; "river - villain donkbet to 30%"; "river - villain donkbet 31%+"] do
       let xlWorkSheet = xlWorkBook.Worksheets.[sheetName] :?> Worksheet 
       let headers = getCellValues xlWorkSheet "AF1" "AZ1"
       let startIndex = headers |> Array.findIndex (fun x -> x = "PATHS") |> (+) 31
       for index in [0..4] do
         let column = excelColumns.[startIndex + index]
         yield!
           getCellValues xlWorkSheet (column + "3") (column + "29")
           |> Array.filter (String.IsNullOrEmpty >> not)
           |> Array.map (fun x -> sheetName + " - " + x.Replace(" ", "").ToLowerInvariant(), index)
    ] |> Map.ofList

  let importRiverIP (xlWorkBook : Workbook) patterns value s h texture =
    let relativeBet = relativeDonkSize s h
    let sheetName = 
      if donkedOnThisStreet s h |> not then "river - villain check"
      elif relativeBet <= 30 then "river - villain donkbet to 30%"
      else "river - villain donkbet 31%+"
    let xlWorkSheet = xlWorkBook.Worksheets.[sheetName] :?> Worksheet 
    let handHasAce = s.Hand.Card1.Face = Ace || s.Hand.Card2.Face = Ace
    let isRiverAce = (Array.last s.Board).Face = Ace
    let turn = s.Board |> Array.take 4
    let turnCard = s.Board.[3]
    let isTurnAce = (Array.last turn).Face = Ace
    let isFlopAce = s.Board |> Array.take 3 |> Array.exists (fun c -> c.Face = Ace)
    let isAceTurnOnly = isTurnAce && not isFlopAce && not isRiverAce
    let isAceRiverOnly = isRiverAce && not isFlopAce && not isTurnAce
    let isAceTurnOrRiver = isTurnAce || isRiverAce
    let isTurnOvercard = isLastBoardCardOvercard turn
    let isRiverOvercard = isLastBoardCardOvercard s.Board
    let areTurnAndRiverOverFlop = isTurnOvercard && isLastBoardCardOvercard (s.Board |> Array.except [turnCard])
    let isRiverMiddlecard = isLastBoardCardSecondCard s.Board
    let isRiverUndercard = isLastBoardCardUndercard s.Board
    let riverDoesNotPair = s.Board |> Array.filter (fun x -> x.Face = s.Board.[4].Face) |> Array.length = 1
    let turnDoesNotPair = s.Board |> Array.filter (fun x -> x.Face = turnCard.Face) |> Array.length = 1
    let highKicker k = k = Ace || k = King || k = Queen || k = Jack
    let topPairedBoard = topPaired s.Board
    let isPairedBoard = isPaired s.Board
    let topPairedBoardOnRiver = topPairedBoard && not isPairedBoard
    let hadPairOnFlop = s.Board |> Array.take 3 |> Array.exists (fun x -> x.Face = s.Hand.Card1.Face || x.Face = s.Hand.Card2.Face)
    let row = 
      match value with
      | Nothing | TwoOvercards -> 
        if handHasAce then 13
        else if isFlopAce && not isTurnAce && not isRiverAce then 5
        elif isAceTurnOnly then 6
        elif isTurnOvercard && isRiverAce && not isTurnAce then 9
        elif isRiverAce && not isFlopAce && not isRiverAce then 7
        elif isTurnOvercard && isRiverOvercard then 8
        elif isTurnOvercard && isRiverMiddlecard then 10
        elif isRiverOvercard && turnDoesNotPair then 11
        elif isTurnOvercard && isRiverUndercard && riverDoesNotPair then 12
        else 4
      | Pair(Over) -> 14
      | Pair(Top _) when isPairedBoard -> 17
      | Pair(Top k) when highKicker k -> 15
      | Pair(Top _) -> 16
      | Pair(Second _) when isTurnAce -> 18
      | Pair(Second _) when isRiverAce -> 19
      | Pair(Second k) when isPairedBoard && isRiverOvercard -> 29
      | Pair(Second k) when isPairedBoard && isTurnOvercard -> 30
      | Pair(Second k) when highKicker k && topPairedBoardOnRiver -> 24
      | Pair(Second k) when topPairedBoardOnRiver -> 25
      | Pair(Second k) when highKicker k && topPairedBoard -> 22
      | Pair(Second k) when topPairedBoard -> 23
      | Pair(Second k) when isRiverOvercard -> 26
      | Pair(Second k) when isTurnOvercard -> 27
      | Pair(Second k) when hadPairOnFlop -> 28
      | Pair(Second k) when highKicker k -> 20
      | Pair(Second _) -> 21
      | Pair(Third _) when isAceTurnOnly -> 32
      | Pair(Third _) when isAceRiverOnly -> 33
      | Pair(Third _) when areTurnAndRiverOverFlop -> 34
      | Pair(Third _) -> 31
      | Pair(Fourth _) when isAceTurnOnly -> 36
      | Pair(Fourth _) when isAceRiverOnly -> 37
      | Pair(Fourth _) when areTurnAndRiverOverFlop -> 38
      | Pair(Fourth _) when isRiverOvercard -> 39
      | Pair(Fourth _) -> 35
      | Pair(Fifth _) when isTurnAce -> 41
      | Pair(Fifth _) when isRiverAce -> 42
      | Pair(Fifth _) when areTurnAndRiverOverFlop -> 43
      | Pair(Fifth _) when isRiverOvercard -> 44
      | Pair(Fifth _) -> 40
      | Pair(Under) when isAceTurnOnly -> 46
      | Pair(Under) when isAceRiverOnly -> 47
      | Pair(Under) -> 45
      | TwoPair -> 
        let pairIndeces = pairIndeces s.Hand s.Board
        let topPair = pairIndeces |> Seq.head = 1
        let topTwoPairs = pairIndeces |> Seq.truncate 2 |> List.ofSeq = [1; 2]

        if topTwoPairs && not isPairedBoard then 48
        elif topPair && not isPairedBoard then 49
        elif topPair && bottomPaired s.Board then 51
        else 50
      | ThreeOfKind -> 52
      | Straight(Normal) -> 53
      | Straight(Weak) -> 54
      | Straight(OnBoard) -> 59
      | Flush(x) -> 
        if texture.Monoboard = 4 then
          match x with
          | Nut -> 67
          | NotNut(King) -> 68
          | NotNut(Queen) -> 69
          | NotNut(Jack) -> 70
          | NotNut(Ten) | NotNut(Nine) -> 71
          | NotNut(Eight) | NotNut(Seven) -> 72
          | NotNut(Six) | NotNut(Five) -> 73
          | _ -> 74
        elif texture.Monoboard = 5 then
          match x with
          | Board -> 67
          | Nut -> 68
          | NotNut(King) -> 69
          | NotNut(Queen) -> 70
          | NotNut(Jack) -> 71
          | NotNut(Ten) | NotNut(Nine) -> 72
          | NotNut(Eight) | NotNut(Seven) -> 73
          | NotNut(Six) | NotNut(Five) -> 74
          | _ -> 75
        else 55
      | FullHouse(Normal) -> 56
      | FullHouse(Weak) -> 57
      | FullHouse(OnBoard) -> 60
      | FourOfKind ->
        if texture.FourOfKind then 
          let kicker = maxFace [s.Hand.Card1; s.Hand.Card2]
          let isNuts = kicker = Ace || (kicker = Ace && s.Board.[0].Face = King)
          if isNuts then 61
          elif kicker = King || kicker = Queen || kicker = Jack then 62 else 63
        else 58
      | StraightFlush -> if texture.Monoboard = 5 then 76 else 58
      |> string

    let columnShift = Map.find (sheetName + " - " + (historyPattern s h)) patterns
    let column =
      match texture.Monoboard, value with
        | 5, _ -> 4
        | 4, Flush(_) -> 2
        | 4, _ -> if texture.Streety then 9 else 8
        | _ ->
          if texture.Streety then 4
          elif texture.DoublePaired then 5
          elif texture.ThreeOfKind then 6
          elif isLastBoardCardFlushy s.Board then 2
          else 1
      |> (+) (columnShift * 10)
      |> List.item <| excelColumns

    let cellValue = getCellValue xlWorkSheet (column + row)
    let optionToParse = if relativeBet > 0 then "ch/" + cellValue else cellValue
    parseOopOption optionToParse ""
    |> Option.add (fun _ -> sheetName + " -> " + column + row)

  let importOopFlop (xlWorkBook : Workbook) sheetName handValue texture =
    let xlWorkSheet = xlWorkBook.Worksheets.[sheetName] :?> Worksheet
    let row = 
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
    let cellValues = getCellValues xlWorkSheet ("B" + row) ("G" + row)
    let (column, specialRulesColumn) = 
      match texture.Monoboard, handValue.FD with
      | 3, NoFD -> (2, 4)
      | 3, Draw(Nut) | 3, Draw(NotNut(King)) | 3, Draw(NotNut(Queen)) | 3, Draw(NotNut(Jack)) -> (5, 4)
      | 3, Draw(_) -> (3, 4)
      | _ -> (0, 1)
    parseOopOption cellValues.[column] cellValues.[specialRulesColumn]
    |> Option.add (fun _ -> sheetName + " -> " + excelColumns.[column + 1] + "/" + excelColumns.[specialRulesColumn + 1] + row)

  let importOopTurn (xlWorkBook : Workbook) sheetName handValue texture =
    let xlWorkSheet = xlWorkBook.Worksheets.[sheetName] :?> Worksheet
    let row = 
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
        | Fifth, _, _ -> failwith "Fifth pair impossible on turn"
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
    let cellValues = getCellValues xlWorkSheet ("K" + row) ("AB" + row)

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
    |> Option.add (fun _ -> sheetName + " -> " + excelColumns.[column + 10] + "/" + excelColumns.[specialColumn + 10] + row)

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
          let topTwoPairs = pairIndeces |> Seq.truncate 2 |> List.ofSeq = [1; 2]

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

    let row = index |> string
    let cellValues = getCellValues xlWorkSheet ("AE" + row) ("AL" + row)

    let (specialConditionsColumn, specialColumn, specialRulesColumn) = 
      match texture.Monoboard with
      | 5 -> (None, 0, None)
      | 4 -> (Some 7, 6, None)
      | _ -> (Some 3, 4, Some 2)
    let sc = Option.map (fun scc -> specialConditionsApply texture cellValues.[scc]) specialConditionsColumn |> defaultArg <| false
    let sr = defaultArg (Option.map (fun src -> cellValues.[src]) specialRulesColumn) ""

    let source o = 
      sheetName + " -> " + excelColumns.[column + 30]
      + if column <> specialColumn then "/" + excelColumns.[specialColumn + 30] else ""
      + row

    parseOopOptionWithSpecialBoard cellValues.[column] sc cellValues.[specialColumn] sr
    |> Option.add source

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
    let importr8r9 () =
      let xlWorkSheet = xlWorkBook.Worksheets.["r8 & r9  riv bet sizings"] :?> Worksheet
      [3..10]
      |> Seq.map (fun row -> getCellValues xlWorkSheet ("A" + row.ToString()) ("K" + row.ToString()))
      |> Seq.takeWhile (fun vs -> not(String.IsNullOrEmpty(vs.[0])))
      |> Seq.map (fun vs ->  
         let potParts = vs.[0].Split([|'-'; '+'|], StringSplitOptions.RemoveEmptyEntries)
         let minPotSize = Int32.Parse(potParts.[0])
         let maxPotSize = if potParts.Length > 1 then Int32.Parse(potParts.[1]) else 1000
         { MinPotSize = minPotSize
           MaxPotSize = maxPotSize
           MinAllInPercentage = parsePercentage vs.[1]
           MaxAllInPercentage = parsePercentage vs.[2]
           BetSize = parsePercentage vs.[3]
           MinChipsLeft = Int32.Parse(vs.[4])
         },
         {
           MinPotSize = minPotSize
           MaxPotSize = maxPotSize
           BetSize = parsePercentage vs.[9]
           ThinBetSize = parsePercentage vs.[10]
         })
      |> List.ofSeq
      |> List.unzip
    let importf1rrrtv () =
      let xlWorkSheet = xlWorkBook.Worksheets.["F1RR & RTV bez sizings"] :?> Worksheet
      [3..20]
      |> Seq.map (fun row -> getCellValues xlWorkSheet ("A" + row.ToString()) ("F" + row.ToString()))
      |> Seq.takeWhile (fun vs -> not(String.IsNullOrEmpty(vs.[0])))
      |> Seq.map (fun vs ->  
         let potParts = vs.[0].Split([|'-'; '+'|], StringSplitOptions.RemoveEmptyEntries)
         let minPotSize = Int32.Parse(potParts.[0])
         let maxPotSize = if potParts.Length > 1 then Int32.Parse(potParts.[1]) else 1000
         { MinPotSize = minPotSize
           MaxPotSize = maxPotSize
           F1RRRatio = parseDecimalThrowing vs.[3]
           RTVRatio = parseDecimalThrowing vs.[5]
         })
      |> List.ofSeq
    match importr8r9() with | (a, b) -> a, b, importf1rrrtv()


  let importFloatFlopOptions (xlWorkBook : Workbook) sheet rangesToCompare s =
    let xlWorkSheet = xlWorkBook.Worksheets.[sheet] :?> Worksheet
    let row = s.Board |> rowIndex |> (+) 5 |> string
    let cellValues = getCellValues xlWorkSheet ("F" + row) ("H" + row)
    let columns = ["F"; "G"; "H"]
    
    rangesToCompare
    |> Seq.map (fun (i, m) -> (Ranges.parseRanges cellValues.[i], m, "tricky -> " + sheet + " -> " + columns.[i] + row))
    |> Seq.filter (fun (r, _, _) -> toHand s.Hand |> Ranges.isHandInRanges r)
    |> Seq.map (fun (_, m, s) -> (m, s))
    |> Seq.tryHead

  let importFloatFlopOopOptions (xlWorkBook : Workbook) s =    
    let villainBetSize = relativeBet s
    let rangesToCompare = seq {
      if villainBetSize = 0 || (villainBetSize >= 26 && villainBetSize <= 57) then yield (0, BluffFloat)
      if villainBetSize = 0 || villainBetSize >= 26 then yield (1, ValueFloat)
    }    
    importFloatFlopOptions xlWorkBook "float OOP" rangesToCompare s
    |> Option.map (fun (m, source) -> { defaultOopOptions with Then = OopOnCBet.Call }, Some(Float m), source)

  let importFloatFlopIpOptions (xlWorkBook : Workbook) s =
    let villainBetSize = relativeBet s
    let rangesToCompare = seq {
      if villainBetSize = 0 || (villainBetSize >= 26 && villainBetSize <= 57) then yield (0, BluffFloat)
      if villainBetSize = 0 || (villainBetSize > 57 && villainBetSize <= 100) then yield (1, BluffFloat)
      if villainBetSize = 0 || villainBetSize >= 26 then yield (2, ValueFloat)
    }
    importFloatFlopOptions xlWorkBook "float IP" rangesToCompare s
    |> Option.map (fun (m, source) -> (OnDonk.Call, OnDonkRaise.Undefined), Some(Float m), source)

  let floatTurnOopColumns value texture s =
    let villainBetHigh = relativeBet s >= 60
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
    yellowColumn, greenColumn

  let floatTurnIpCheckColumns value texture s h =
    let villainBetFlop = h |> List.filter (fun x -> x.Street = Flop) |> List.map (fun x -> x.VsVillainBet) |> List.head
    let potBeforeFlop = s.Pot - s.VillainBet - villainBetFlop
    let villainFlopBetHigh = villainBetFlop * 100 / potBeforeFlop >= 57
    let trinityCheck a b c = if texture.Streety then c elif villainFlopBetHigh then b else a
    let yellowColumnCheck = 
      match texture.Monoboard, value.FD with
      | 3, NoFD -> trinityCheck "T" "U" "V"
      | 3, Draw(Nut) -> trinityCheck "X" "Y" "Z"
      | 3, Draw(NotNut k) when k = King || k = Queen -> trinityCheck "AA" "AB" "AC"
      | 3, Draw(NotNut k) when k = Jack || k = Ten -> trinityCheck "AD" "AE" "AF"
      | 3, Draw(NotNut x) when x = Nine || x = Eight || x = Seven -> trinityCheck "AG" "AH" "AI"
      | 3, Draw(NotNut x) -> trinityCheck "AJ" "AK" "AL"
      | 3, Draw(Board) -> failwith "Board draw impossible on 3 monoboard"
      | _ ->
        if texture.Streety then "P" 
        elif texture.DoublePaired then "Q"
        elif texture.ThreeOfKind then "R"
        elif villainFlopBetHigh then "O"
        else "N"
    let greenColumnCheck = 
      match texture.Monoboard, value.FD with
      | 3, NoFD -> "AP"
      | 3, Draw(Nut) -> "AR"
      | 3, Draw(NotNut k) when k = King || k = Queen -> "AS"
      | 3, Draw(NotNut k) when k = Jack || k = Ten -> "AT"
      | 3, Draw(NotNut x) when x = Nine || x = Eight || x = Seven -> "AU"
      | 3, Draw(NotNut x) -> "AV"
      | 3, Draw(Board) -> failwith "Board draw impossible on 3 monoboard"
      | _ -> "AN"
    yellowColumnCheck, greenColumnCheck

  let floatTurnIpDonkColumns value texture s =
    let villainTurnBetHigh = relativeBet s >= 58
    let trinityDonk a b c = if texture.Streety then c elif villainTurnBetHigh then b else a
    let yellowColumnDonk = 
      match texture.Monoboard, value.FD with
      | 3, NoFD -> if texture.Streety then "BG" else "BF"
      | 3, Draw(Nut) -> trinityDonk "BI" "BJ" "BK"
      | 3, Draw(NotNut k) when k = King || k = Queen -> trinityDonk "BL" "BM" "BN"
      | 3, Draw(NotNut k) when k = Jack || k = Ten -> trinityDonk "BO" "BP" "BQ"
      | 3, Draw(NotNut x) when x = Nine || x = Eight || x = Seven -> trinityDonk "BR" "BS" "BT"
      | 3, Draw(NotNut x) -> trinityDonk "BU" "BV" "BW"
      | 3, Draw(Board) -> failwith "Board draw impossible on 3 monoboard"
      | _ ->
        if texture.Streety then "BB" 
        elif texture.DoublePaired then "BC"
        elif texture.ThreeOfKind then "BD"
        else "AZ"

    let greenColumnDonk = 
      match texture.Monoboard, value.FD with
      | 3, NoFD -> "CA"
      | 3, Draw(Nut) -> if villainTurnBetHigh then "CD" else "CC"
      | 3, Draw(NotNut k) when k = King || k = Queen -> if villainTurnBetHigh then "CF" else "CE"
      | 3, Draw(NotNut k) when k = Jack || k = Ten -> if villainTurnBetHigh then "CH" else "CG"
      | 3, Draw(NotNut x) when x = Nine || x = Eight || x = Seven -> if villainTurnBetHigh then "CJ" else "CI"
      | 3, Draw(NotNut x) -> if villainTurnBetHigh then "CL" else "CK"
      | 3, Draw(Board) -> failwith "Board draw impossible on 3 monoboard"
      | _ -> "BY"

    yellowColumnDonk, greenColumnDonk

  let importFloatTurnOptions xlWorkSheet value texture s h (yellowColumn, greenColumn) =
    let villainBet = relativeBet s
    let row = turnDonkOrFloatOopRow 5 value s texture

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
    |> Option.map (fun col -> col + row, getCellValue xlWorkSheet (col + row))
    |> Option.map (fun (source, cell) -> (cell, motivation cell, source))

  let importFloatTurnOopOptions (xlWorkBook : Workbook) value texture s h =
    let xlWorkSheet = xlWorkBook.Worksheets.["float OOP"] :?> Worksheet
    let yellowColumn, greenColumn = floatTurnOopColumns value texture s
    importFloatTurnOptions xlWorkSheet value texture s h (yellowColumn, greenColumn)
    |> Option.bind (fun (cell, motivation, source) -> 
      match parseOopOption cell "" with
      | Some o -> Some (o, motivation, "tricky -> float OOP -> " + source)
      | None -> None)

  let importFloatTurnIpCheckOptions (xlWorkBook : Workbook) value texture s h =
    let xlWorkSheet = xlWorkBook.Worksheets.["float IP"] :?> Worksheet
    let yellowColumn, greenColumn = floatTurnIpCheckColumns value texture s h
    importFloatTurnOptions xlWorkSheet value texture s h (yellowColumn, greenColumn)
    |> Option.bind (fun (cell, motivation, source) -> 
      let motiv o = if String.IsNullOrEmpty(o.Scenario) then motivation else Some(Motivation.Scenario(o.Scenario))
      match parseOopOption cell "" with
      | Some o -> Some (o, motiv o, "tricky -> float IP -> " + source)
      | None -> None)

  let importFloatTurnIpDonkOptions (xlWorkBook : Workbook) value texture s h =
    let xlWorkSheet = xlWorkBook.Worksheets.["float IP"] :?> Worksheet
    let yellowColumn, greenColumn = floatTurnIpDonkColumns value texture s
    importFloatTurnOptions xlWorkSheet value texture s h (yellowColumn, greenColumn)
    |> Option.map (fun (cell, motivation, source) -> (parseTurnDonk cell, motivation, "tricky -> float IP -> " + source))

  let importFloatRiverOptions (xlWorkBook : Workbook) sheetName column value texture s h =
    let xlWorkSheet = xlWorkBook.Worksheets.[sheetName] :?> Worksheet
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
    let hadPairOnFlop = s.Board |> Array.take 3 |> Array.exists (fun x -> x.Face = s.Hand.Card1.Face || x.Face = s.Hand.Card2.Face)
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
      | Pair(Second k) when hadPairOnFlop -> 23
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
        let topTwoPairs = pairIndeces |> Seq.truncate 2 |> List.ofSeq = [1; 2]

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
    let continuation =  
      h 
      |> List.tryLast 
      |> Option.bind (fun hi -> match hi.Motivation with | Some(Float(WithContinuation x)) -> Some (x, "", "continuation") | _ -> None)
    
    let villainBet = relativeBet s
    let specialColumn =
      if column = "AX" then Some "AY"
      elif column = "CP" then Some "CQ"
      else None
    let column = 
      if villainBet > 0 && villainBet < 26 then None
      else Some column

    let readColumn () =      
      let special = specialColumn |> Option.map (fun x -> getCellValue xlWorkSheet (x + row))
      column 
      |> Option.map (fun col -> getCellValue xlWorkSheet (col + row), defaultArg special "", col + row)

    continuation |> orElse readColumn

  let importFloatRiverOopOptions (xlWorkBook : Workbook) value texture s h =
    let column = 
      match texture.Monoboard, value with
      | 4, Flush(_) -> "AX"
      | 4, _ -> if texture.Streety then "BE" else "BD"
      | _ ->
        if texture.Streety then "AZ" 
        elif texture.DoublePaired then "BA"
        elif texture.ThreeOfKind then "BB"
        else "AX"
    importFloatRiverOptions xlWorkBook "float OOP" column value texture s h
    |> Option.bind (fun (c1, c2, source) -> parseOopOption c1 c2 |> Option.map (fun x -> x, "tricky -> float OOP -> " + source))

  let importFloatRiverIpCheckOptions (xlWorkBook : Workbook) value texture s h =
    let column = 
      match texture.Monoboard, value with
      | 4, Flush(_) -> "CP"
      | 4, _ -> if texture.Streety then "CW" else "CV"
      | _ ->
        if texture.Streety then "CR" 
        elif texture.DoublePaired then "CS"
        elif texture.ThreeOfKind then "CT"
        else "CP"
    importFloatRiverOptions xlWorkBook "float IP" column value texture s h
    |> Option.bind (fun (c1, c2, source) -> 
      parseOopOption c1 c2 |> Option.map (fun x -> x, "tricky -> float IP -> " + source))

  let importFloatRiverIpDonkOptions (xlWorkBook : Workbook) value texture s h =
    let column = 
      match texture.Monoboard, value with
      | 4, Flush(_) -> "CZ"
      | 4, _ -> if texture.Streety then "DF" else "DE"
      | _ ->
        if texture.Streety then "DA" 
        elif texture.DoublePaired then "DB"
        elif texture.ThreeOfKind then "DC"
        else "CZ"
    importFloatRiverOptions xlWorkBook "float IP" column value texture s h
    |> Option.map (fun (c1, _, source) -> parseTurnDonk c1, "tricky -> float IP -> " + source)

  let importIPTurnBooster (xlWorkBook : Workbook) value texture s h =
    let xlWorkSheet = xlWorkBook.Worksheets.["postflop IP turn booster"] :?> Worksheet
    let row = turnDonkOrFloatOopRow 5 value s texture
    let extra = match h with | { Action = Action.Call; VsVillainBet = v } :: _ when v > s.BB -> 7 | _ -> 0
    let column =
      if texture.Streety then 5
      elif texture.DoublePaired then 6
      elif texture.ThreeOfKind then 7
      else 2
      |> (+) extra
      |> List.item <| excelColumns
    let cellValue = getCellValue xlWorkSheet (column + row)
    parseOopOption cellValue ""
    |> Option.addValue ("HandStrength -> postflop IP turn booster -> " + column + row)
    
  let importFlopCbetMixup (xlWorkBook : Workbook) s h =
    let xlWorkSheet = xlWorkBook.Worksheets.["cbet mix up"] :?> Worksheet
    let row = rowIndex s.Board + 2 |> string
    let isInRanged r = toHand s.Hand |> Ranges.isHandInRanges r
    let noCbet r = if isInRanged r then Some defaultIpOptions else None
    let bettingRange column r = 
      if isInRanged r then 
        getCellValue xlWorkSheet (column + "459")
        |> parseDecimal
        |> Option.map (fun d -> { defaultIpOptions with CbetFactor = Always d })
      else Some defaultIpOptions
    match h with
    | [{Action = RaiseToAmount _}] -> Some ("E", noCbet)
    | [{Action = Action.Call; VsVillainBet = x}] when x = s.BB -> Some ("F", noCbet)
    | [{Action = Action.Call; VsVillainBet = x}] when x >= s.BB * 4 -> Some ("G", bettingRange "G")
    | [{Action = Action.Call}] -> Some ("H", bettingRange "H")
    | _ -> None
    |> Option.mapFst (fun c -> c + row)
    |> Option.map (fun (cell, f) -> (cell, getCellValue xlWorkSheet cell, f))
    |> Option.bind (fun (cell, cellValue, f) -> 
      Ranges.parseRanges cellValue 
      |> f 
      |> Option.add (fun _ -> "HandStrength -> cbet mix up -> " + cell))

  let importCbetMixupCheckRaise (xlWorkBook : Workbook) s handValue =
    let xlWorkSheet = xlWorkBook.Worksheets.["flop hand strength"] :?> Worksheet
    let overs = overcards s.Hand s.Board
    let handHasAce = s.Hand.Card1.Face = Ace || s.Hand.Card2.Face = Ace
    let aceOnFlop = s.Board |> Array.exists (fun x -> x.Face = Ace)
    let singleAceOnFlop = s.Board |> Array.filter (fun x -> x.Face = Ace) |> Array.length = 1
    let highKicker k = k = Ace || k = King || k = Queen || k = Jack
    let topPairedBoard = topPaired s.Board
    let pairedBoard = pairFace s.Board |> Option.isSome
    let highBoardPair = pairFace s.Board |> Option.filter highKicker |> Option.isSome
    let row = 
      (match handValue.Made with
      | Nothing
      | TwoOvercards ->
        match handValue.FD2, handValue.SD with
        | Draw(_), OpenEnded -> 62
        | Draw(_), GutShot -> 63
        | Draw(_), NoSD when overs = 2 -> 60
        | Draw(_), NoSD when overs = 1 -> 59
        | Draw(_), NoSD -> 58
        | NoFD, OpenEnded when overs = 2 -> 49
        | NoFD, OpenEnded when overs = 1 -> 48
        | NoFD, OpenEnded when aceOnFlop -> 47
        | NoFD, OpenEnded -> 46
        | NoFD, GutShot when overs = 2 -> 37
        | NoFD, GutShot when overs = 1 -> 36
        | NoFD, GutShot when aceOnFlop -> 35
        | NoFD, GutShot -> 34
        | NoFD, NoSD when overs = 2 -> 8
        | NoFD, NoSD when overs = 1 -> 7
        | NoFD, NoSD when singleAceOnFlop -> 6
        | NoFD, NoSD when handHasAce -> 5
        | NoFD, NoSD -> 4
      | Pair(x) -> 
        match x, handValue.FD2, handValue.SD with
        | Over, _, _ -> 10
        | Top(_), Draw(_), _ -> 61
        | Top(_), _, OpenEnded -> 50
        | Top(_), _, GutShot -> 38
        | Top(k), NoFD, NoSD when highBoardPair -> 13
        | Top(k), NoFD, NoSD when highKicker k -> 11
        | Top(_), NoFD, NoSD -> 12
        | Second(_), Draw(_), _  -> 64
        | Second(_), _, OpenEnded when aceOnFlop -> 52
        | Second(_), _, OpenEnded -> 51
        | Second(_), _, GutShot when aceOnFlop -> 40
        | Second(_), _, GutShot -> 39
        | Second(k), NoFD, NoSD when highKicker k && topPairedBoard -> 14
        | Second(_), NoFD, NoSD when topPairedBoard -> 15
        | Second(k), NoFD, NoSD when highKicker k && pairedBoard -> 17
        | Second(k), NoFD, NoSD when highKicker k -> 16
        | Second(k), NoFD, NoSD when aceOnFlop -> 19
        | Second(_), NoFD, NoSD -> 18
        | Third, Draw(_), _ -> 65
        | Third, _, OpenEnded when aceOnFlop -> 54
        | Third, _, OpenEnded -> 53
        | Third, _, GutShot when aceOnFlop -> 42
        | Third, _, GutShot -> 41
        | Third, NoFD, NoSD when pairedBoard -> 22
        | Third, NoFD, NoSD when aceOnFlop -> 21
        | Third, NoFD, NoSD -> 20
        | Fourth, _, _ -> failwith "Fourth pair impossible on flop"
        | Fifth, _, _ -> failwith "Fifth pair impossible on flop"
        | Under, _, OpenEnded when aceOnFlop -> 56
        | Under, _, OpenEnded -> 55
        | Under, _, GutShot when aceOnFlop -> 44
        | Under, _, GutShot -> 43
        | Under, _, _ when aceOnFlop -> 24
        | Under, _, _ -> 23
      | TwoPair -> 25
      | ThreeOfKind -> 26
      | Straight(_) -> 27
      | Flush(_) -> 28
      | FullHouse(x) ->
        match tripsFace s.Board with
        | Some(k) -> 
          let kicker = concat s.Hand s.Board |> anyPairFace |> Option.get
          if highKicker kicker then 31 else 30
        | None -> 29
      | StraightFlush | FourOfKind -> 32
      )|> string
    let column = if s.VillainStack = 0 then "C" else "B"
    let cellValue = getCellValue xlWorkSheet (column + row)
    
    { defaultIpOptions with CheckRaise = parseCheckRaise cellValue }, "HandStrength -> flop hand strength -> " + column + row
