namespace Player

open Akka.FSharp
open Cards
open Click
open Stats
open Import
open PostFlop.Import
open Preflop
open System.Drawing
open Hands
open Cards.HandValues
open Actions
open Recognition.ScreenRecognition
open PostFlop.HandValue
open PostFlop.Decision
open PostFlop.Facade
open Excel.Import

module Decide =
  open Interaction

  let fileNameIP = System.IO.Directory.GetCurrentDirectory() + @"\IPinput.xlsx"
  let rulesIP = importExcel (importRulesByStack importRulesIP) fileNameIP
  let fileNameOOP = System.IO.Directory.GetCurrentDirectory() + @"\OOPinput.xlsx"
  let rulesOOP = importExcel (importRulesByStack importRulesOOP) fileNameOOP
  let fileNameAdvancedOOP = System.IO.Directory.GetCurrentDirectory() + @"\PostflopPART2.xlsx"
  let (rulesAdvancedOOP, hudData, bluffyCheckRaiseFlopsLimp, bluffyCheckRaiseFlopsMinr, bluffyOvertaking, bluffyHandsForFlopCheckRaise, notOvertakyHandsInLimpedPot, riverBetSizes) = 
    importExcel (fun x -> (importOopAdvanced x, 
                                   importHudData x, 
                                   importFlopList "bluffy hero ch-r flop vs limp" x,
                                   importFlopList "bluffy hero ch-r flop vs minr" x,
                                   importFlopList "bluffy overtaking, vill ch b fl" x,
                                   importRange "herBLUF ch-r flop vsCALL minrPR" 2 x,
                                   importRange "extras" 1 x,
                                   importRiverBetSizes x)) fileNameAdvancedOOP
  let isHandBluffyForFlopCheckRaise hand = 
    let ranges = Ranges.parseRanges bluffyHandsForFlopCheckRaise
    Ranges.isHandInRanges ranges (toHand hand)
  let isHandOvertakyInLimpedPot hand =
    let ranges = Ranges.parseRanges notOvertakyHandsInLimpedPot
    Ranges.isHandInRanges ranges (toHand hand) |> not
  let rulesLow = List.concat [rulesIP; rulesAdvancedOOP.Always; rulesAdvancedOOP.LimpFoldLow; rulesOOP]
  let rulesBig = List.concat [rulesIP; rulesAdvancedOOP.Always; rulesAdvancedOOP.LimpFoldBig; rulesOOP]
  let decidePre stack odds limpFold = 
    if limpFold >= 65 then decideOnRules rulesBig stack odds
    else decideOnRules rulesLow stack odds

  let understandHistory (screen: Screen) =
    let raise bet bb = 
      let b = (bet |> decimal) / (bb |> decimal)
      WasRaise(b)
    let villainAllIn = screen.VillainStack = Some 0
    match screen.Button, screen.Blinds, screen.VillainBet, screen.HeroBet with
    | Hero, Some {BB = bb; SB = sb}, Some vb, Some hb when vb <= bb && sb = hb -> []
    | Hero, Some {BB = bb}, Some vb, Some hb when vb > bb && bb = hb -> [WasLimp; raise vb bb]
    | Hero, Some {BB = bb}, Some vb, Some hb when hb > bb && vb > hb && villainAllIn -> [raise hb bb; WasRaiseAllIn]
    | Hero, Some {BB = bb}, Some vb, Some hb when hb > bb && vb > hb -> [raise hb bb; raise vb bb]
    | Villain, Some {BB = bb}, Some vb, Some hb when vb = bb && hb = bb -> [WasLimp]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb = bb && vb > bb && villainAllIn -> [WasRaiseAllIn]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb = bb && vb > bb -> [raise vb bb]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb > bb && hb < 4 * bb && vb > hb && villainAllIn -> [WasLimp; raise hb bb; WasRaiseAllIn]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb > bb && hb < 4 * bb && vb > hb -> [WasLimp; raise hb bb; raise vb hb]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb > bb && vb > hb && villainAllIn -> [raise (hb * 2 / 5) bb; raise 5 2; WasRaiseAllIn]
    | Villain, Some {BB = bb}, Some vb, Some hb when hb > bb && vb > hb -> [raise (hb * 2 / 5) bb; raise 5 2; raise vb hb]
    | _ -> failwith "History is not clear"

  let decide' log xlFlopTurn xlTurnDonkRiver xlPostFlopOop xlTricky riverHistoryPatterns (screen: Screen) history: MotivatedAction option =
    let decidePre (screen: Screen) =
      match screen.HeroStack, screen.HeroBet, screen.VillainStack, screen.VillainBet, screen.Blinds with
      | Some hs, Some hb, Some vs, Some vb, Some b -> 
        let stack = min (hs + hb) (vs + vb)
        let effectiveStack = decimal stack / decimal b.BB
        let callSize = min (vb - hb) hs
        let potOdds = (callSize |> decimal) * 100m / (vb + hb + callSize |> decimal) |> ceil |> int
        let hudStats = hud hudData screen.VillainName
        let openRaise = (if b.BB >= 20 then hudStats.OpenRaise20_25 else if b.BB >= 16 then hudStats.OpenRaise16_19 else hudStats.OpenRaise14_15) |> decimal
        let fullHand = parseFullHand screen.HeroHand
        let history = understandHistory screen
        let actionPattern = decidePre effectiveStack potOdds hudStats.LimpFold openRaise history fullHand
        Option.map (mapPatternToAction PreFlop vb stack) actionPattern  
      | _ -> None
    let decidePost (screen: Screen) =
      match screen.TotalPot, screen.HeroStack, screen.VillainStack, screen.Blinds with
      | Some tp, Some hs, Some vs, Some b -> 
        let suitedHand = screen.HeroHand |> parseSuitedHand
        let board = screen.Board |> parseBoard
        let value = handValueWithDraws suitedHand board
        log (sprintf "Hand value: %A" value)
        let special = boardTexture board
        let vb = defaultArg screen.VillainBet 0
        let hb = defaultArg screen.HeroBet 0
        let s = { Hand = suitedHand; Board = board; Pot = tp; VillainStack = vs; HeroStack = hs; VillainBet = vb; HeroBet = hb; BB = b.BB }
        if screen.Button = Hero then 
          decidePostFlop history s value special xlFlopTurn xlTurnDonkRiver xlTricky riverBetSizes riverHistoryPatterns
        else
          decidePostFlopOop history s value special xlPostFlopOop xlTricky (bluffyCheckRaiseFlopsLimp, bluffyCheckRaiseFlopsMinr, bluffyOvertaking) (isHandBluffyForFlopCheckRaise, isHandOvertakyInLimpedPot) riverBetSizes
      | _ -> None

    match screen.Sitout, screen.Board with
    | Villain, _ -> Action.RaiseToAmount 1 |> (notMotivated (streetOfBoard screen.Board) 0) |> Some
    | Hero, _ -> Action.SitBack |> (notMotivated PreFlop 0) |> Some
    | _, null -> decidePre screen
    | _, _ -> decidePost screen

  type DecisionMessage = {
    WindowTitle: string
    TableName: string
    Screen: Screen
    Bitmap: Bitmap
  }

  type DecisionState = {
    LastScreen: Screen
    PreviousActions: MotivatedAction list
  }

  let mapAction action minBet buttons : ClickAction[] =
    let findButton names =
      names 
      |> List.choose (fun x -> Seq.tryFind (fun y -> x = y.Name) buttons)
      |> List.tryHead
    let button =
      match action with
      | Action.Fold -> ["Check"; "Fold"]
      | Action.Check -> ["Check"]
      | Action.Call -> ["Call"; "AllIn"]
      | Action.RaiseToAmount _ -> ["RaiseTo"; "Bet"; "AllIn"; "Call"]
      | Action.AllIn -> ["AllIn"; "RaiseTo"; "Bet"; "Call"]
      | Action.SitBack -> ["SitBack"]
      |> findButton

    match (action, button) with
    | (Action.AllIn, Some b) when b.Name <> "AllIn" -> [|Click({ Region = (368, 389, 42, 7); Name = "Max" }); Click(b)|]
    | (Action.RaiseToAmount x, Some b) when x > minBet -> [| Amount(x); Click(b)|]
    | (_, Some b) -> [|Click(b)|]
    | (_, None) -> failwith "Could not find an appropriate button"

  let dump title msg =
    let filename = sprintf "%s_%s_%s_%s" title msg.TableName msg.Screen.HeroHand msg.Screen.Board
    Dumper.SaveBitmap(msg.Bitmap, filename, true)

  let decisionActor xlFlopTurn xlHandStrength xlPostFlopOop xlTricky riverHistoryPatterns msg (state:DecisionState option) =
    let log (s: string) =
      System.IO.File.AppendAllLines(sprintf "%s.log" msg.TableName, [s])
      System.Console.WriteLine(s)
    let pushAction state action reset =
      match state, action with
      | s, Some { Action = SitBack; Motivation = _ } -> defaultArg (Option.map (fun x -> x.PreviousActions) s) []
      | Some s, Some a -> if reset then [a] else List.append s.PreviousActions [a]
      | Some s, None -> s.PreviousActions
      | None, Some a -> [a]
      | None, None -> []

    let screen = msg.Screen
    match state with
    | Some s when s.LastScreen = screen -> (None, state)
    | Some s when 
      not (System.String.IsNullOrEmpty screen.Board) &&
      match s.PreviousActions |> List.tryLast |> Option.map (fun a -> a.VsVillainBet), screen.VillainBet with
      | Some pvb, Some vb when pvb = vb && screen.Board = s.LastScreen.Board -> true
      | _ -> false
      -> 
      print screen |> List.iter (sprintf "%s: %s" "Hand" >> log)
      log "Looks like we already made action for this decision, dumping the screenshot..."
      dump "Dup" msg
      (None, state)
    | _ ->
      print screen |> List.iter (sprintf "%s: %s" "Hand" >> log)
      let isPre = System.String.IsNullOrEmpty screen.Board
      let history = if isPre then [] else Option.map (fun s -> s.PreviousActions) state |> defaultArg <| []
//      let history = [
//        {Action = Call; Motivation = None; VsVillainBet = 30; Street = PreFlop; Source = null;} 
//      ]
      history |> List.iter (sprintf "History: %A" >> log)

      try
        let decision = decide' log xlFlopTurn xlHandStrength xlPostFlopOop xlTricky riverHistoryPatterns screen history
        let newState = Some { LastScreen = screen; PreviousActions = pushAction state decision isPre }
        match decision with
        | Some d ->
          sprintf "Decision is: %A" d |> log
          let minBet = 
            screen.Blinds
            |> Option.map (fun x -> x.SB)
            |> defaultArg <| 0
            |> defaultArg screen.VillainBet
            |> (*) 2
          let action = mapAction d.Action minBet screen.Actions
          sprintf "Action is: %A" action |> log
          if (d.Action = SitBack) then
            dump "SitBack" msg
          let clickMsg = { WindowTitle = msg.WindowTitle; Clicks = action; IsInstant = screen.Sitout <> Unknown; Screen = screen }
          let statsMsg = { StatsMessage.Source = d.Source }
          (Some (clickMsg, statsMsg), newState)
        | None ->
          log "Could not make a decision, dumping the screenshot..."
          dump "Dec" msg
          (None, newState)
      with ex ->
        sprintf "Error occured: %s %s" ex.Message ex.StackTrace |> log
        dump "Error" msg
        (None, state)