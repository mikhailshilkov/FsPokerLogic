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
  | Limp
  | Raise of decimal * decimal
  | RaiseEQ of int
  | RaiseFor3BetShove of decimal * decimal // calling range * opening range threshold
  | RaiseAllIn

type DecisionRule = 
  { StackRange : int * int
    Range : string
    History : HistoryItem seq
    Action : ActionPattern }

let isHistoryMatching ranges history stack odds openingRange =
  let if3BetShove raiseX callingRange openingRange allinPot =
    (((-((((raiseX+1m))*((100m-((100m*callingRange)/openingRange))/100m))))*(openingRange/callingRange)+((allinPot/2m)-1m))*100m)/allinPot

  (Seq.compareWith (fun r h -> 
    match (r, h) with
    | Limp, Limp -> 0
    | RaiseAllIn, RaiseAllIn -> 0
    | Raise (min, max), Raise (v, _) -> if min <= v && v <= max then 0 else 1
    | RaiseEQ eq, Raise (_, _) -> if eq >= odds then 0 else 1
    | RaiseFor3BetShove(cra, orathres), Raise (v, _) -> if if3BetShove v cra openingRange (stack*2m) > orathres then 0 else 1
    | _ -> 1) ranges history) = 0

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
  |> Seq.tryPick (fun r -> if isMatching r (normalize h) then Some(r.Action) else None)