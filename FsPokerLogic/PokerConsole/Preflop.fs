module Preflop

open Hands
open Ranges

type Action = 
  | AllIn
  | MinRaise
  | Call
  | Fold

type HistoryItem = 
  | Limp
  | Raise of int * int
  | RaiseAllIn

type DecisionRule = 
  { StackMin: int
    StackMax: int
    Range : string
    History : HistoryItem seq
    Action : Action }

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
    isHandInRanges ranges h 
      && rule.StackMin <= stack 
      && stack <= rule.StackMax 
      && (isHistoryMatching rule.History history)
  rules
  |> Seq.filter (fun r -> isMatching r h)
  |> Seq.map (fun r -> r.Action)
  |> Seq.tryHead