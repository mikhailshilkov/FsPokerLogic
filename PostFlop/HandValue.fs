namespace PostFlop

module HandValue =
  open Hands
  open Options
  open Cards.Actions
  open Cards.HandValues
  open PostFlop.Decision

  let isSecondPairWithAKKicker handValue =
    match handValue with
    | Pair(Second(King)) | Pair(Second(Ace)) -> true
    | _ -> false

  let isSingleCardFlushDrawWithAKQKicker (hand: SuitedHand) (board: Board) =
    [hand.Card1; hand.Card2]
    |> Seq.exists (fun x -> faceValue x.Face >= 12 
                            && Seq.filter (fun y -> y.Suit = x.Suit) board |> Seq.length = 3)

  let is3SuitedCardsOnBoard board = 
    board
    |> Seq.countBy (fun x -> x.Suit)
    |> Seq.map snd
    |> Seq.sort
    |> Seq.last = 3

  let oldRules s handValue o =
    let street = street s
    if street = Flop && s.VillainStack = 0 && isSecondPairWithAKKicker handValue then 
      {o with Donk = OnDonk.CallRaisePet } 
    else if street = Turn && o.CheckRaise = OnCheckRaise.Call && is3SuitedCardsOnBoard s.Board && not(isSingleCardFlushDrawWithAKQKicker s.Hand s.Board) then
      {o with CheckRaise = OnCheckRaise.StackOff } 
    else o

  let cbetFlushDrawOnTurn s handValue texture o =
    if street s = Turn && isFlushDrawWith2 s.Hand s.Board  then
      let overs = overcards s.Hand s.Board
      let hasAceOrLastOver = 
        isLastBoardCardOvercard s.Board 
        || s.Board |> Array.filter (fun c -> c.Face = Ace) |> Array.length = 1
      let betNeeded =
        (handValue.Made <> Nothing || handValue.SD <> NoSD || overs > 0 || hasAceOrLastOver)
        && not texture.Streety && not texture.DoublePaired && not texture.ThreeOfKind
      if betNeeded then
        let cr = 
          match handValue.Made with
          | Nothing | TwoOvercards | Pair(Under)
          | Pair(Fifth) | Pair(Fourth) | Pair(Third) -> OnCheckRaise.Call
          | _ -> OnCheckRaise.StackOff
        { o with CbetFactor = OrAllIn { DefaultCBetOr with Factor = 75m; IfRemainingChipsLessThan = 99 }; CheckRaise = cr } 
      else
        { o with CbetFactor = Never } 
    else o

  let bluffMissedFlushDrawOnRiver s value history o =
    let turnBoard = boardAtStreet Turn s.Board
    let hasAceOrTurnOrRiverIsOver = 
      s.Board |> Array.filter (fun c -> c.Face = Ace) |> Array.length = 1
      || isLastBoardCardOvercard s.Board 
      || isLastBoardCardOvercard turnBoard
    let isLimpPre = List.tryHead history = Some Action.Call
    let stackPre = effectiveStackPre s
    if street s = River && isFlushDrawWith2 s.Hand turnBoard
      && match List.tryLast history with | Some(Action.RaiseToAmount _) -> true | _ -> false
      && hasAceOrTurnOrRiverIsOver 
      && (stackPre >= 20 || stackPre >= 12 && isLimpPre)
      && match value with | Nothing | Pair(Under) | Pair(Fifth) | Pair(Fourth) | Pair(Third) -> true | _ -> false
      then { o with CbetFactor = OrAllIn { DefaultCBetOr with Factor = 70m; IfRemainingChipsLessThan = 79 }; CheckRaise = OnCheckRaise.CallEQ 5 }
    else o

  let augmentOptions s handValue texture history o =
    let rules = [oldRules s handValue.Made; 
      cbetFlushDrawOnTurn s handValue texture;
      bluffMissedFlushDrawOnRiver s handValue.Made history]
    rules |> List.fold (fun opt rule -> rule opt) o