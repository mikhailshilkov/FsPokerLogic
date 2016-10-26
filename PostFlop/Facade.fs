namespace PostFlop

open Microsoft.FSharp.Core
open Hands
open Cards.HandValues
open Decision
open Options
open Import
open Monoboard
open Texture
open HandValue
open SpecialRules

module Facade =
  open Cards.Actions

  let defaultArgLazy o p = match o with | Some v -> v | None -> p()
  let orElse b a = match a with | Some _ -> a | None -> b()

  let floatedBefore h = h |> List.exists (fun hi -> match hi.Motivation with | Some(Float _) -> true | _ -> false)

  let toMotivated s (d, m, source) =
    d |> Option.map (fun a -> { MotivatedAction.Action = a; Motivation = m; VsVillainBet = s.VillainBet; Street = street s; Source = source }) 

  let toNotMotivated s (d, source) =
    d |> Option.map (fun a -> { MotivatedAction.Action = a; Motivation = None; VsVillainBet = s.VillainBet; Street = street s; Source = source }) 

  let canFloatIp s h =
    effectiveStackPre s >= 10 && match List.tryHead h with | Some x when x.VsVillainBet = s.BB -> true | _ -> false

  let canFloatFlop s value texture = s.HeroBet = 0 && value.FD2 = NoFD && texture.Monoboard < 3

  let decideFlopCbetMixup xlHandStrength history s value () = 
    let mixupedBefore = 
      history 
      |> List.tryLast 
      |> Option.bind (fun x -> Option.ofString x.Source) 
      |> Option.exists (fun x -> x.Contains("cbet mix up"))
    match street s, value.FD, s.VillainBet with
      | Flop, NoFD, 0 ->
        importFlopCbetMixup xlHandStrength s history
        |> Option.mapFst (fun o -> decide [] s history o)
        |> Option.bind (toNotMotivated s)
      | Flop, NoFD, x when x > 0 && mixupedBefore ->
        let (o, source) = importCbetMixupCheckRaise xlHandStrength s value
        decide [] s history o
        |> Option.map (fun action -> { Action = action; Motivation = None; VsVillainBet = s.VillainBet; Street = street s; Source = source })
      | _ -> None

  let decidePostFlopFloatOnDonk riverBetSizes history s value texture xlTricky () =
    let float = 
      if donkedOnThisStreet s history && canFloatIp s history then
        match street s with
        | Flop when canFloatFlop s value texture -> importFloatFlopIpOptions xlTricky s
        | Turn when floatedBefore history -> importFloatTurnIpDonkOptions xlTricky value texture s history
        | River when floatedBefore history -> 
          importFloatRiverIpDonkOptions xlTricky value.Made texture s history
          |> Option.map (fun (x, source) -> x, None, source)
        | _ -> None
      else None
    float
    |> Option.map (fun (o, m, source) -> ({ CbetFactor = Never; CheckRaise = OnCheckRaise.Undefined; Donk = fst o; DonkRaise = snd o }, m, source))
    |> Option.map (fun (o, m, source) -> (decide riverBetSizes s history o, m, source))
    |> Option.bind (toMotivated s)

  let decidePostFlopFloatOnCheck history s value texture xlTricky riverBetSizes () =
    let float = 
      if donkedOnThisStreet s history |> not && canFloatIp s history then
        match street s with
        | Turn when floatedBefore history -> importFloatTurnIpCheckOptions xlTricky value texture s history
        | River when floatedBefore history -> 
          importFloatRiverIpCheckOptions xlTricky value.Made texture s history
          |> Option.map (fun (x, source) -> (x, None, source))
        | _ -> None
      else None
    float
    |> Option.map (fun (o, m, source) -> scenarioRulesOop s history o, m, source)
    |> Option.map (fun (o, m, source) -> decideOop riverBetSizes s o, m, source)
    |> Option.bind (toMotivated s)

  let decidePostFlopTurnBooster history s value texture xl riverBetSizes eo () =
    let playedBoosterOrBluffBet = history |> List.exists (fun hi -> hi.Source <> null && (hi.Source.Contains("IP turn booster") || hi.Source.Contains("PostflopIP -> O")))
    match street s with
    | Turn when s.VillainBet = 0 || playedBoosterOrBluffBet ->
      let o = eo |> Option.map (fun eov -> toTurnOptions s.Board value OnDonk.Undefined OnDonkRaise.Undefined 0 eov)
      match o with
      | Some (ov, _) when ov.CbetFactor = Never || (s.VillainBet > 0 && ov.CheckRaise = OnCheckRaise.Fold) ->
        importIPTurnBooster xl value texture s history
        |> Option.map (fun (o, source) -> decideOop riverBetSizes s o, None, source)
        |> Option.bind (toMotivated s)
      | _ -> None
    | _ -> None

  let decidePostFlopRiver h s value texture xlHandStrength riverBetSizes patterns () =
    match street s with
    | River -> 
      importRiverIP xlHandStrength patterns value s h texture
      |> Option.mapFst (scenarioRulesOop s h)
      |> Option.map (fun (o, source) -> decideOop riverBetSizes s o, None, source)
      |> Option.bind (toMotivated s)
    | _ -> None

  let decidePostFlopNormal riverBetSizes history s value texture xlFlopTurn xlTurnDonkRiver eo =
    let historyTuples = List.map (fun x -> (x.Action, x.Motivation)) history
    let historySimple = List.map fst historyTuples

    let (options, motivation, source) =
      match street s, eo with
      | Turn, Some eoo ->
        let (turnDonkOption, turnDonkRaiseOption, motivation, source) = 
          if s.VillainBet > 0 
          then importTurnDonk xlTurnDonkRiver value texture s history 
          else (OnDonk.Undefined, OnDonkRaise.Undefined, None, "PostflopIP")
        let o, column =
          toTurnOptions s.Board value turnDonkOption turnDonkRaiseOption texture.Monoboard eoo
          |> (fun (op, c) -> if texture.Monoboard >= 3 then monoboardTurn texture.Monoboard value op, "Mono" else op, c)
        o, motivation, source + (if s.VillainBet > 0 then "" else " -> " + column)
      | Flop, Some eoo ->
        let o =
          toFlopOptions (isFlushDrawWith2 s.Hand s.Board) (canBeFlushDraw s.Board) eoo
          |> (if texture.Monoboard >= 3 then monoboardFlop value else id)
        o, None, "PostflopIP"
      | _ -> failwith "Failed to produce IP options"
    
    options
    |> augmentOptions s value texture historySimple
    |> decide riverBetSizes s history
    |> Option.map (fun x -> x, motivation, source)

  let apply f = f()
  let decidePostFlop history s value texture xlFlopTurn xlHandStrength xlTricky riverBetSizes riverHistoryPatterns =
    let eo = 
      match street s with
      | Turn | Flop -> 
        let limpedPot = match history with | { Action = Action.Call } :: _ -> true | _ -> false
        importOptions xlFlopTurn s.Hand s.Board limpedPot |> Some
      | _ -> None

    let rules = [
      decideFlopCbetMixup xlHandStrength history s value;
      decidePostFlopFloatOnDonk (riverBetSizes |> Tuple.thrd3) history s value texture xlTricky;
      decidePostFlopFloatOnCheck history s value texture xlTricky riverBetSizes;
      decidePostFlopTurnBooster history s value texture xlHandStrength riverBetSizes eo;
      decidePostFlopRiver history s value.Made texture xlHandStrength riverBetSizes riverHistoryPatterns;
      fun () -> 
        decidePostFlopNormal (riverBetSizes |> Tuple.thrd3) history s value texture xlFlopTurn xlHandStrength eo
        |> Option.map (fun (a, m, source) -> { Action = a; Motivation = m; VsVillainBet = s.VillainBet; Street = street s; Source = source })
    ]
    rules |> Seq.choose apply |> Seq.tryHead

  let rec pickOopSheet history s =
    match history with
    | (Action.Check, _)::_ -> (Some "limp and check", true)
    | (Action.Call, _)::_ -> (Some "hero call raise pre", true)
    | (Action.RaiseToAmount a, Some Bluff) :: _ when a < s.BB * 4 -> (Some "hero raise FB vs limp", false)
    | (Action.RaiseToAmount a, None) :: _ when a < s.BB * 4 -> (Some "hero raise FV vs limp", false)
    | (Action.RaiseToAmount a, Some Bluff) :: _ -> (Some "hero 3b chips FB vs minr", false)
    | (Action.RaiseToAmount _, _) :: _ -> (Some "hero 3b chips FV vs minr", false)
    | (Action.SitBack, _)::rem -> pickOopSheet rem s
    | [] when s.Pot = s.VillainBet + s.HeroBet + 2 * s.BB -> (Some "limp and check", true)
    | [] -> (Some "hero call raise pre", true)
    | _ -> (None, false)

  let decidePostFlopOop history s value texture xlOop xlTricky bluffyFlops bluffyHand riverBetSizes =
    let historyTuples = List.map (fun x -> (x.Action, x.Motivation)) history
    let historySimple = List.map fst historyTuples
    let (preflopPattern, preflopAllowsFloat) = pickOopSheet historyTuples s

    let float = 
      if preflopAllowsFloat && effectiveStackPre s >= 10 then
        match street s with
        | Flop when s.VillainBet > 0 && canFloatFlop s value texture -> 
          importFloatFlopOopOptions xlTricky s
        | Turn when floatedBefore history -> importFloatTurnOopOptions xlTricky value texture s history
        | River when floatedBefore history -> 
          importFloatRiverOopOptions xlTricky value.Made texture s history 
          |> Option.map (fun (o, source) -> specialRulesOop s history o, None, source)
        | _ -> None
        |> Option.map (fun (o, m, source) -> scenarioRulesOop s history o, m, source)
      else None

    let normalPlay() =
      match street s, preflopPattern with
      | Flop, Some p -> importOopFlop xlOop p value texture
      | Turn, Some p -> importOopTurn xlOop p value texture
      | River, Some p -> importOopRiver xlOop p value.Made texture s
      | _ -> failwith "Unkown street at decidePostFlopOop"
      |> Option.mapFst (specialRulesOop s history)
      |> Option.mapFst (scenarioRulesOop s history)
      |> Option.mapFst (strategicRulesOop s value history bluffyFlops bluffyHand)
      |> Option.map (fun ((a, b), c) -> a, b, "PostflopOOP -> " + c)

    float
    |> orElse normalPlay
    |> Option.map (fun (o, m, source) -> (decideOop riverBetSizes s o, m, source))
    |> Option.bind (toMotivated s)