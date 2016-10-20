namespace PostFlop

open Cards.Actions
open Cards.HandValues
open Hands
open Options
open Decision
open PostFlop.Parsing

module SpecialRules = 
  let specialRulesOop s history o = 
    let checkCheckMatch f t other =       
      match previousStreetHistory s history |> List.map (fun h -> h.Action) with
      | [Action.Check] -> { o with First = f; Then = t; Scenario = o.SpecialScenario } 
      | _ -> other

    let rec imp remaining =
      match remaining with
      | CallEQPlusXvsAI dx::rem ->
        match o.Then, s.VillainStack with
        | CallEQ x, 0 -> { o with Then = CallEQ (x + dx); Scenario = o.SpecialScenario }
        | CallEQIfRaised(r, nr), 0 -> { o with Then = CallEQIfRaised(r + dx, nr + dx); Scenario = o.SpecialScenario }
        | Raise(r, CallEQ x), 0 -> { o with Then = Raise(r, CallEQ(x + dx)); Scenario = o.SpecialScenario }
        |_ -> imp rem
      | PairedBoard(f, t)::rem ->
        if isPaired s.Board then { o with First = f; Then = t; Scenario = o.SpecialScenario } else imp rem
      | BoardOvercard(f, t)::rem -> 
        if isLastBoardCardOvercard s.Board then { o with First = f; Then = t; Scenario = o.SpecialScenario } else imp rem
      | BoardAce(f,t)::rem ->
        if (Array.last s.Board).Face = Ace 
          && s.Board |> Array.filter (fun x -> x.Face = Ace) |> Array.length = 1 then 
          { o with First = f; Then = t; Scenario = o.SpecialScenario } 
        else imp rem
      | CheckCheck(f, t)::rem -> checkCheckMatch f t (imp rem)
      | CheckCheckAndBoardOvercard(f, t)::rem -> 
        if isLastBoardCardOvercard s.Board then checkCheckMatch f t (imp rem) else imp rem
      | KHighOnPaired::rem ->
        if o.Then = Fold && isPaired s.Board && isXHigh King s.Hand && s.BB <= 30 then 
          { o with Then = CallEQ (if s.BB = 20 then 30 else 25); Scenario = o.SpecialScenario } 
        else imp rem
      | CheckRaiseOvercardBluff(t)::rem ->
        let villainBet = relativeBet s
        if isLastBoardCardOvercard s.Board 
          && (villainBet = 0 || villainBet >= 35 && villainBet <= 56) 
          && stackIfCall s >= s.BB * 8
        then { o with First = Check; Then = t; Scenario = o.SpecialScenario } else imp rem
      | NotUsed::rem -> imp rem
      | [] -> o
    imp o.Special

  // Game Plan OOP -> Main rule 2
  let overtakeLimpedPot overtakyHand s value h o =
    let monoboardAtFlop = monoboardLength (s.Board |> Array.take 3)
    if List.tryHead h = Some Action.Check
      && boardAtStreet Flop s.Board |> Array.forall (fun x -> x.Face <> Ace) 
      && monoboardAtFlop < 3
      && s.BB <= 30
      && effectiveStackPre s >= 12
      && (value.Made <> Nothing || value.SD = OpenEnded || overtakyHand s.Hand)
    then 
      if street s = Turn 
         && List.tryLast h = Some Action.Check then { o with First = Donk 75m }
      else 
      if street s = River 
         && match List.tryLast h with | Some(Action.RaiseToAmount x) -> x >= s.Pot * 3 / 10 - 1 | _ -> false
      then { o with First = Donk 50m } 
      else o
    else o

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

  let flopMatches s flops =
    let flopString = s.Board |> Array.take 3 |> Array.map (fun c -> c.Face) |> Array.sortBy faceValue |> List.ofArray
    flops |> List.exists (fun x -> x = flopString)

  let bluffyCheckRaiseFlopInLimpedPotFlop flops s value history o =
    let weakHand = match value with | Nothing | TwoOvercards | Pair(Under) | Pair(Fifth) | Pair(Fourth) | Pair(Third) -> true | _ -> false
    match street s, s.BB, potPre s, o.Then, weakHand with
    | Flop, 20, 40, Fold, true when effectiveStackPre s >= 15 && flopMatches s flops && s.VillainBet > 0 && s.VillainBet <= 40 -> 
      let raiseSize = 
        match s.VillainBet with
        | 40 -> 3m
        | x when x >= 31 && x <= 39 -> 3.5m
        | _ -> 4m
      { o with Then = Raise(raiseSize, OopOnCBet.Fold) }
    | _ -> o

  let bluffyCheckRaiseFlopInLimpedPotTurnRiver flops s value history o =
    let limpedPre = match List.tryHead history with | Some { Action = Action.Check } -> true | _ -> false
    let lastActionBluff = match List.tryLast history with | Some { Action = RaiseToAmount(_); Motivation = Some Bluff } -> true | _ -> false
    match street s, s.BB with
    | Turn, 20 when lastActionBluff && limpedPre && flopMatches s flops
      -> { o with First = OopDonk.Donk 62.5m }
    | River, 20 when lastActionBluff && limpedPre && flopMatches s flops 
                  && (isLastBoardCardOvercard (s.Board |> Array.take 4) || isLastBoardCardOvercard s.Board)
      -> { o with First = OopDonk.AllIn }
    | _ -> o

  let bluffyCheckRaiseInRaisedPot bluffyHand flops s value history o =
    match street s, s.BB, s.Pot, s.VillainBet with
    | Flop, 20, 120, 40
      when effectiveStackPre s >= 18 && flopMatches s flops && (bluffyHand s.Hand || value.Made = TwoOvercards || value.SD = GutShot)
      -> { o with Then = Raise(2.75m, OopOnCBet.Fold) }
    | Turn, 20, 300, 0
      when stack s >= 210 && flopMatches s flops
           && (value.Made <> Nothing || value.FD <> NoFD || value.SD <> NoSD || isLastBoardCardOvercard s.Board) 
           && history |> List.exists (fun ma -> ma.Motivation = Some Bluff)
      -> { o with First = OopDonk.AllIn }
    | _ -> o

  let bluffyOvertakingTurn flops s value history o = 
    let weakHand = 
      match value.Made with | Nothing | TwoOvercards -> true | _ -> false
      && value.SD <> OpenEnded
      && value.FD = NoFD
    let lastCard = Array.last s.Board |> (fun x -> x.Face)
    match street s, history with
    | Turn, [Action.Call; Action.Check]
      when weakHand && effectiveStackPre s >= 18 && lastCard <> Ace && flopMatches s flops
      -> { o with First = OopDonk.Donk 75m }
    | _ -> o

  let bluffyOvertakingRiver flops s history o = 
    let lastCard = Array.last s.Board |> (fun x -> x.Face)
    let lastActionBluff = match List.tryLast history with | Some { Action = RaiseToAmount(_); Motivation = Some Bluff } -> true | _ -> false
    let raisedPot = match List.tryHead history with | Some { Action = Action.Call; Motivation = _ } -> true | _ -> false
    match street s with
    | River when lastCard <> Ace && flopMatches s flops && lastActionBluff && raisedPot
      -> { o with First = OopDonk.Donk 62.5m }
    | _ -> o

  let strategicRulesOop s value history (bluffyCheckRaiseFlopsLimp, bluffyCheckRaiseFlopsMinr, bluffyOvertaking) (bluffyHand, overtakyHand) o =
    let historySimple = List.map (fun x -> x.Action) history
    let motivationSeed = if o.Scenario <> null then Some(Scenario(o.Scenario)) else None
    let rules = [
      (overtakeLimpedPot overtakyHand s value historySimple, None);
      (allInTurnAfterCheckRaiseInLimpedPot s historySimple, None);
      (checkCallPairedTurnAfterCallWithSecondPairOnFlop s value.Made historySimple, None);
      (bluffyCheckRaiseFlopInLimpedPotFlop bluffyCheckRaiseFlopsLimp s value.Made history, Some Bluff);
      (bluffyCheckRaiseFlopInLimpedPotTurnRiver bluffyCheckRaiseFlopsLimp s value.Made history, Some Bluff);
      (bluffyCheckRaiseInRaisedPot bluffyHand bluffyCheckRaiseFlopsMinr s value history, Some Bluff);
      (bluffyOvertakingTurn bluffyOvertaking s value historySimple, Some Bluff);
      (bluffyOvertakingRiver bluffyOvertaking s history, Some Bluff)
    ]
    rules |> List.fold (fun (options, motivation) (rule, ruleMotiv) -> 
                          let newO = rule options
                          if newO <> options then (newO, ruleMotiv) else (options, motivation)) (o, motivationSeed)

  let scenarioRulesOop s history o =
    if o.Scenario = null then o
    else
      let parts = o.Scenario.Split([|'/'|], 2)
      let motivationOfLastAction = previousStreetHistory s history |> List.tryLast |> Option.bind (fun x -> x.Motivation)
      match motivationOfLastAction, parts with
      | Some(Motivation.Scenario(x)), [|p1; Int i|] when p1.Contains(x) -> { o with First = RiverBetSizing; Then = CallEQ i }
      | _ -> o