module Preflop

open Hands
open Ranges

type ActionPattern = 
  | AllIn
  | MinRaise
  | RaiseX of decimal
  | Call
  | Check
  | Fold

type RelativePosition = IP | OOP

type HistoryItem = 
  | WasLimp
  | WasRaise of decimal
  | WasRaiseAllIn

type RuleHistoryItem = 
  | Limp
  | Raise of decimal * decimal
  | RaiseEQ of int
  | RaiseFor3BetShove of decimal * decimal // calling range * opening range threshold
  | BluffableRaise
  | RaiseAllIn

type DecisionRule = 
  { StackRange : int * int
    Range : string
    History : RuleHistoryItem seq
    Action : ActionPattern }

type VillainStats =
  { VillainName: string
    OpenRaise20_25: int
    OpenRaise16_19: int
    OpenRaise14_15: int
    LimpFold: int }

let hud (data: VillainStats list) villainName = 
  let fuzzyNameMatch (parsed: string) full = 
    if parsed = full then true
    else
      let parsedPart = parsed.Replace("?", "")
      parsedPart.Length >= 7 && full.IndexOf(parsedPart) > 0
  let matching = data |> List.tryFind (fun s -> fuzzyNameMatch villainName s.VillainName)
  defaultArg matching (List.head data)

let isHistoryMatching ranges history stack odds openingRange =
  let if3BetShove raiseX callingRange openingRange allinPot =
    (((-((((raiseX+1m))*((100m-((100m*callingRange)/openingRange))/100m))))*(openingRange/callingRange)+((allinPot/2m)-1m))*100m)/allinPot

  if Seq.length ranges = Seq.length history 
  then
    Seq.zip ranges history
    |> Seq.forall (fun (r, h) -> 
      match (r, h) with
      | Limp, WasLimp -> true
      | RaiseAllIn, WasRaiseAllIn -> true
      | Raise (min, max), WasRaise v -> min <= v && v <= max
      | RaiseEQ eq, WasRaise _ -> eq >= odds
      | RaiseFor3BetShove(cra, orathres), WasRaise v -> if3BetShove v cra openingRange (stack*2m) < orathres
      | BluffableRaise, WasRaise v -> openingRange > 60m && v = 2m
      | _ -> false)
  else false

let decideOnRules rules stack odds openingRange history h = 
  let isMatching rule h = 
    let ranges = parseRanges rule.Range
    let (stackMin, stackMax) = rule.StackRange
    let (stackMinF, stackMaxF) = (stackMin |> decimal, stackMax |> decimal)
    isHandInRanges ranges h 
      && stackMinF - 0.5m <= stack 
      && stack <= stackMaxF + 0.5m 
      && (isHistoryMatching rule.History history stack odds openingRange)
  rules
  |> Seq.filter (fun r -> isMatching r (normalize h))    
  |> Seq.tryHead |> Option.map (fun x -> x.Action)