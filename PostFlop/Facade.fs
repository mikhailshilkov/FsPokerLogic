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

  let canFloatIp s h =
    effectiveStackPre s >= 10 && match List.tryHead h with | Some x when x.VsVillainBet = s.BB -> true | _ -> false

  let decidePostFlopFloatOnDonk history s value texture xlTricky () =
    let float = 
      if s.VillainBet > 0 && canFloatIp s history then
        match street s with
        | Flop when s.HeroBet = 0 -> importFloatFlopIpOptions xlTricky s
        | Turn when floatedBefore history -> importFloatTurnIpDonkOptions xlTricky value texture s history
        | River when floatedBefore history -> 
          importFloatRiverIpDonkOptions xlTricky value.Made texture s history
          |> Option.map (fun (x, source) -> x, None, source)
        | _ -> None
      else None
    float
    |> Option.map (fun (o, m, source) -> ({ CbetFactor = Never; CheckRaise = OnCheckRaise.Call; Donk = fst o; DonkRaise = snd o }, m, source))
    |> Option.map (fun (o, m, source) -> (decide s history o, m, source))
    |> Option.bind (toMotivated s)

  let decidePostFlopFloatOnCheck history s value texture xlTricky riverBetSizes () =
    let float = 
      if s.VillainBet = 0 && canFloatIp s history then
        match street s with
        | Turn when floatedBefore history -> importFloatTurnIpCheckOptions xlTricky value texture s history
        | River when floatedBefore history -> 
          importFloatRiverIpCheckOptions xlTricky value.Made texture s history
          |> Option.map (fun (x, source) -> (x, None, source))
        | _ -> None
      else None
    float
    |> Option.map (fun (o, m, source) -> scenarioRulesOop history o, m, source)
    |> Option.map (fun (o, m, source) -> decideOop riverBetSizes s o, m, source)
    |> Option.bind (toMotivated s)

  let decidePostFlopTurnBooster history s value texture xl riverBetSizes eo () =
    let o = eo |> Option.map (fun eov -> toTurnOptions s.Board value OnDonk.Undefined OnDonkRaise.Undefined 0 eov)
    match street s, s.VillainBet, o with
    | Turn, vb, Some ov 
      when ov.CbetFactor = Never || (vb > 0 && ov.CheckRaise = OnCheckRaise.Fold) ->
      importIPTurnBooster xl value texture s history
      |> Option.map (fun (o, source) -> decideOop riverBetSizes s o, None, source)
      |> Option.bind (toMotivated s)
    | _ -> None

  let decidePostFlopNormal history s value texture xlFlopTurn xlTurnDonkRiver eo =
    let historyTuples = List.map (fun x -> (x.Action, x.Motivation)) history
    let historySimple = List.map fst historyTuples

    (match street s, eo with
    | River, _ ->
      let mono = if texture.Monoboard >= 4 then monoboardRiver texture.Monoboard value.Made else None
      defaultArgLazy mono (fun x -> importRiver xlTurnDonkRiver texture value.Made)
    | Turn, Some eoo ->
      let (turnDonkOption, turnDonkRaiseOption) = 
        if s.VillainBet > 0 
        then importTurnDonk xlTurnDonkRiver value texture s history 
        else (OnDonk.Undefined, OnDonkRaise.Undefined)
      toTurnOptions s.Board value turnDonkOption turnDonkRaiseOption texture.Monoboard eoo
      |> (if texture.Monoboard >= 3 then monoboardTurn texture.Monoboard value else id)
    | Flop, Some eoo ->
      toFlopOptions (isFlushDrawWith2 s.Hand s.Board) (canBeFlushDraw s.Board) eoo
      |> (if texture.Monoboard >= 3 then monoboardFlop value else id)
    | _ -> failwith "Failed to produce IP options"
    )
    |> augmentOptions s value texture historySimple
    |> decide s history

  let apply f = f()
  let decidePostFlop history s value texture xlFlopTurn xlTurnDonkRiver xlTricky riverBetSizes =
    let eo = 
      match street s with
      | Turn | Flop -> 
        let limpedPot = match history with | { Action = Action.Call } :: _ -> true | _ -> false
        importOptions xlFlopTurn s.Hand s.Board limpedPot |> Some
      | _ -> None

    let rules = [
      decidePostFlopFloatOnDonk history s value texture xlTricky;
      decidePostFlopFloatOnCheck history s value texture xlTricky riverBetSizes;
      decidePostFlopTurnBooster history s value texture xlTurnDonkRiver riverBetSizes eo;
      fun () -> 
        decidePostFlopNormal history s value texture xlFlopTurn xlTurnDonkRiver eo
        |> Option.map (fun a -> { Action = a; Motivation = None; VsVillainBet = s.VillainBet; Street = street s; Source = if s.VillainBet > 0 then "HandStrength" else "PostflopIP" })
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
        | Flop when s.VillainBet > 0 && s.HeroBet = 0 -> importFloatFlopOopOptions xlTricky s
        | Turn when floatedBefore history -> importFloatTurnOopOptions xlTricky value texture s history
        | River when floatedBefore history -> 
          importFloatRiverOopOptions xlTricky value.Made texture s history 
          |> Option.map (fun (o, source) -> specialRulesOop s history o, None, source)
        | _ -> None
        |> Option.map (fun (o, m, source) -> scenarioRulesOop history o, m, source)
      else None

    let normalPlay() =
      match street s, preflopPattern with
      | Flop, Some p -> importOopFlop xlOop p value texture
      | Turn, Some p -> importOopTurn xlOop p value texture
      | River, Some p -> importOopRiver xlOop p value.Made texture s
      | _ -> failwith "Unkown street at decidePostFlopOop"
      |> Option.mapFst (specialRulesOop s history)
      |> Option.mapFst (scenarioRulesOop history)
      |> Option.mapFst (strategicRulesOop s value history bluffyFlops bluffyHand)
      |> Option.map (fun ((a, b), c) -> a, b, "PostflopOOP -> " + c)

    float
    |> orElse normalPlay
    |> Option.map (fun (o, m, source) -> (decideOop riverBetSizes s o, m, source))
    |> Option.bind (toMotivated s)