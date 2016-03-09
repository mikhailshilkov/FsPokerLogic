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
  | RaiseAllIn

type DecisionRule = 
  { Stack : int
    Range : string
    History : HistoryItem seq
    Action : ActionPattern }

let isHistoryMatching ranges history =
  (Seq.compareWith (fun r h -> 
    match (r, h) with
    | Limp, Limp -> 0
    | RaiseAllIn, RaiseAllIn -> 0
    | Raise (min, max), Raise (v, _) -> if min <= v && v <= max then 0 else 1
    | _ -> 1) ranges history) = 0

let decideOnRules rules stack history h = 
  let isMatching rule h = 
    let ranges = parseRanges rule.Range
    let stackFloat = rule.Stack |> decimal
    isHandInRanges ranges h 
      && stackFloat - 0.5m <= stack 
      && stack <= stackFloat + 0.5m 
      && (isHistoryMatching rule.History history)
  rules
  |> Seq.tryPick (fun r -> if isMatching r (normalize h) then Some(r.Action) else None)