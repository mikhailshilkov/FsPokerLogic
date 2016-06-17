namespace PostFlop

open Cards.Actions
open Cards.HandValues
open Hands
open Options
open Decision


module SpecialRules = 
  let specialRulesOop s history o = 
    let rec imp remaining =
      match remaining with
      | CallEQPlusXvsAI dx::rem ->
        match o.Then, s.VillainStack with
        | CallEQ x, 0 -> { o with Then = CallEQ (x + dx) }
        |_ -> imp rem
      | PairedBoard(f, t)::rem ->
        if isPaired s.Board then { o with First = f; Then = t } else imp rem
      | BoardOvercard(f, t)::rem -> 
        if isLastBoardCardOvercard s.Board then { o with First = f; Then = t } else imp rem
      | BoardAce(f,t)::rem ->
        if (Array.last s.Board).Face = Ace 
          && s.Board |> Array.filter (fun x -> x.Face = Ace) |> Array.length = 1 then 
          { o with First = f; Then = t } 
        else imp rem
      | CheckCheck(f, t)::rem ->
        if List.tryLast history = Some Action.Check then { o with First = f; Then = t } else imp rem
      | CheckCheckAndBoardOvercard(f, t)::rem ->
        if List.tryLast history = Some Action.Check && isLastBoardCardOvercard s.Board then { o with First = f; Then = t } else imp rem
      | KHighOnPaired::rem ->
        if o.Then = Fold && isPaired s.Board && isXHigh King s.Hand && s.BB <= 30 then 
          { o with Then = CallEQ (if s.BB = 20 then 30 else 25) } 
        else imp rem
      | CheckRaiseBluffOnFlop:: rem ->
        let isBlufyBoard board =
          let bluffyFlops = ["2 5 7"; "2 5 8"; "2 5 9"; "2 6 7"; "2 6 8"; "2 6 9"; "2 6 T"; "2 7 9"; "2 8 J"; "3 3 8"; "3 3 9"; "3 4 7"; "3 4 8"; "3 4 9"; "3 5 6"; "3 5 7"; "3 5 8"; "3 5 9"; "3 5 T"; "3 6 7"; "3 6 8"; "3 6 9"; "3 7 9"; "3 7 T"; "3 7 J"; "3 8 T"; "3 8 J"; "4 4 6"; "4 4 7"; "4 4 8"; "4 4 9"; "4 5 7"; "4 5 8"; "4 5 9"; "4 5 T"; "4 5 J"; "4 6 7"; "4 6 8"; "4 6 9"; "4 6 T"; "4 6 J"; "4 7 9"; "4 7 T"; "4 7 J"; "4 8 T"; "4 8 J"; "4 9 J"; "4 T Q"; "5 5 7"; "5 5 9"; "5 5 T"; "5 6 8"; "5 6 9"; "5 6 T"; "5 6 J"; "5 6 Q"; "5 7 7"; "5 7 8"; "5 7 9"; "5 7 T"; "5 7 J"; "5 7 Q"; "5 8 T"; "5 8 Q"; "5 9 J"; "5 9 Q"; "5 T Q"; "5 J Q"; "6 6 9"; "6 6 T"; "6 6 J"; "6 7 T"; "6 7 Q"; "6 8 T"; "6 8 Q"; "6 T Q"; "6 J Q"; "7 7 T"; "7 7 J"; "7 8 J"; "7 8 Q"; "7 T Q"; "7 J Q"; "8 8 J"; "8 J J"]
          let cards = board |> Seq.take 3 |> Seq.map (fun x -> x.Face) |> Seq.sortBy faceValue |> Seq.map faceToChar
          let key = System.String.Join(" ", cards)
          List.contains key bluffyFlops
        if s.BB = 20 && potPre s = 4 * s.BB && effectiveStackPre s >= 18 && s.VillainBet * 3 = s.Pot
          && (isGutShot s.Hand s.Board || overcards s.Hand s.Board >= 1) && isBlufyBoard s.Board then
          { o with First = Check; Then = RaiseFold } 
        else imp rem
      | [] -> o
    imp o.Special

  // Game Plan OOP -> Main rule 2
  let mainRule2 s h texture o =
    if List.tryHead h = Some Action.Check
      && boardAtStreen Flop s.Board |> Array.forall (fun x -> x.Face <> Ace) 
      && texture.Monoboard < 3
      && s.BB <= 30
      && effectiveStackOnCurrentStreet s >= s.BB * 12
    then 
      if street s = Turn 
         && List.tryLast h = Some Action.Check then { o with First = Donk 75m }
      else 
      if street s = River 
         && match List.tryLast h with | Some(Action.RaiseToAmount x) -> x >= s.Pot * 3 / 10 - 1 | _ -> false
      then { o with First = Donk 50m } 
      else o
    else o

  let increaseTurnBetEQvsAI s o =
    match street s, o.First, o.Then, s.VillainStack with
      | Turn, Donk _, CallEQ x, 0 -> { o with Then = CallEQ (x + 6) }
      | _ -> o

  let allInTurnAfterCheckRaiseInLimpedPot s h o =
    match street s, h with
      | Turn, Action.Check :: Action.Check :: RaiseToAmount x :: [] when x + stack s <= 11 * s.BB -> { o with First = OopDonk.AllIn }
      | _ -> o

  let checkCallPairedTurnAfterCallWithSecondPairOnFlop s value h o =
    let turnDuplicated() =
      let maxFlopCard = s.Board |> Array.take 3 |> Array.maxBy (fun x -> faceValue x.Face)
      let turnCard = s.Board.[3].Face
      maxFlopCard.Face = turnCard
    match street s, value, List.tryLast h with
    | Turn, Pair(Second(_)), Some(Action.Call) when turnDuplicated() -> { o with First = Check; Then = Call }
    | _ -> o

  let bluffyCheckRaiseFlop flops s value o =
    let flopAndStack () =
      let flopString = s.Board |> Array.take 3 |> Array.map (fun c -> c.Face) |> Array.sortBy faceValue |> List.ofArray
      flops |> List.exists (fun x -> x = flopString)
    match street s, s.BB, s.Pot, s.VillainBet, o.Then with
    | Flop, 20, 120, 40, Fold when effectiveStackPre s >= 18 && flopAndStack() -> { o with Then = RaiseFold }
    | Turn, 20, 300, 0, _ when stack s >= 210 && flopAndStack() && (value.Made <> Nothing || value.FD <> NoFD || value.SD <> NoSD || isLastBoardCardOvercard s.Board) 
      -> { o with First = OopDonk.AllIn }
    | _ -> o

  let strategicRulesOop s value h texture bluffyCheckRaiseFlops o =
    let rules = [
      mainRule2 s h texture
      increaseTurnBetEQvsAI s
      allInTurnAfterCheckRaiseInLimpedPot s h
      checkCallPairedTurnAfterCallWithSecondPairOnFlop s value.Made h
      bluffyCheckRaiseFlop bluffyCheckRaiseFlops s value
    ]
    rules |> List.fold (fun options rule -> rule options) o