namespace PostFlop

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

  let decidePostFlop history s value texture xlFlopTurn xlTurnDonkRiver =
    let historyTuples = List.map (fun x -> (x.Action, x.Motivation)) history
    let historySimple = List.map fst historyTuples
    let limpedPot = match historySimple with | Action.Call :: _ -> true | _ -> false
    (match street s with
    | River ->
      let mono = if texture.Monoboard >= 4 then monoboardRiver texture.Monoboard value.Made else None
      defaultArgLazy mono (fun x -> importRiver xlTurnDonkRiver texture value.Made)
    | Turn ->
      let eo = importOptions xlFlopTurn s.Hand s.Board limpedPot
      let turnFace = s.Board.[3].Face
      let (turnDonkOption, turnDonkRaiseOption) = 
        if s.VillainBet > 0 
        then importTurnDonk xlTurnDonkRiver value texture s history 
        else (OnDonk.Undefined, OnDonkRaise.Undefined)
      toTurnOptions turnFace (match value.Made with | Flush(_) -> true | _ -> false) turnDonkOption turnDonkRaiseOption texture.Monoboard eo
      |> (if texture.Monoboard >= 3 then monoboardTurn texture.Monoboard value else id)
    | Flop ->
      let eo = importOptions xlFlopTurn s.Hand s.Board limpedPot
      toFlopOptions (isFlushDrawWith2 s.Hand s.Board) (canBeFlushDraw s.Board) eo
      |> (if texture.Monoboard >= 3 then monoboardFlop value else id)
    | PreFlop -> failwith "We are not playing preflop here"
    )
    |> augmentOptions s value texture historySimple
    |> decide s history

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
    let (preFlopPattern, canFloat) = pickOopSheet historyTuples s

    let float = 
      if canFloat && effectiveStackPre s >= 10 then
        match street s with
        | Flop -> importFloatFlopOopOptions xlTricky s
        | Turn -> importFloatTurnOopOptions xlTricky value texture s history
        | River -> importFloatRiverOptions xlTricky value.Made texture s history |> Option.map (fun v -> (v, None))
        | _ -> failwith "Unkown street at decidePostFlopOop"
      else None

    let normalPlay() =
      match street s, preFlopPattern with
      | Flop, Some p -> importOopFlop xlOop p value texture
      | Turn, Some p -> importOopTurn xlOop p value texture
      | River, Some p -> importOopRiver xlOop p value.Made texture s
      | _ -> failwith "Unkown street at decidePostFlopOop"
      |> Option.map (specialRulesOop s historySimple)
      |> Option.map (scenarioRulesOop history)
      |> Option.map (strategicRulesOop s value history bluffyFlops bluffyHand)

    float
    |> orElse normalPlay
    |> Option.map (fun (o, m) -> (decideOop riverBetSizes s o, m))
    |> Option.bind (fun (ao, m) -> ao |> Option.map (fun a -> { MotivatedAction.Action = a; Motivation = m; VsVillainBet = s.VillainBet; Street = street s }))