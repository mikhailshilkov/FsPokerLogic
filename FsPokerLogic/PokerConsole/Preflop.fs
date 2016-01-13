module Preflop

open Hands
open Ranges

type Action = 
  | AllIn
  | MinRaise
  | Call
  | Fold

let decideByRanges isFold isLimp isRaise isAllIn hand : Action = 
  if isAllIn hand then AllIn
  else if isRaise hand then MinRaise
  else if isLimp hand then Call
  else if isFold hand then Fold
  else failwith "Can not make a decision"

let never hand = false

let decideRaiseRaise6x h = 
  let rangeFold = 
    parseRanges 
      "K7s-K2s,Q9s-Q2s,J7s-J4s,T7s-T4s,97s-94s,86s-84s,75s-74s,64s+,54s,A7o-A5o,K9o-K3o,Q8o-Q5o,J8o-J6o,T7o-T6o,97o-96o,86o+,76o"
  let rangeCall = parseRanges "K9s-K8s,87s,76s"
  let rangeAllIn = parseRanges "99+,A5s+,A8o+"
  decideByRanges (isHandInRanges rangeFold) (isHandInRanges rangeCall) never (isHandInRanges rangeAllIn) h

let decideRaiseRaise8x h = 
  let rangeFold = 
    parseRanges 
      "K9s-K2s,Q9s-Q2s,J7s-J4s,T7s-T4s,97s-94s,84s+,74s+,64s+,54s,A7o-A5o,K9o-K3o,Q8o-Q5o,J8o-J6o,T7o-T6o,97o-96o,86o+,76o"
  let rangeAllIn = parseRanges "99+,A5s+,A8o+"
  decideByRanges (isHandInRanges rangeFold) never never (isHandInRanges rangeAllIn) h

let decideRaiseAllIn h = 
  let rangeFold = 
    parseRanges 
      "A7s-A5s,K9s-K2s,Q9s-Q2s,J7s-J4s,T7s-T4s,97s-94s,84s+,74s+,64s+,54s,A9o-A5o,K9o-K3o,Q8o-Q5o,J8o-J6o,T7o-T6o,97o-96o,86o+,76o"
  let rangeCall = parseRanges "99+,A8s+,ATo+"
  decideByRanges (isHandInRanges rangeFold) (isHandInRanges rangeCall) never never h

type HistoryItem = 
  | Limp
  | Raise of int * int

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

let decide stack history h = 
  let rules = 
    [| { StackMin = 0
         StackMax = 12
         History = []
         Range = "J3s-J2s,T3s-T2s,93s-92s,83s-82s,73s-72s,63s-62s,53s-52s,42s+,32s,K2o,Q4o-Q2o,J5o-J2o,T5o-T2o,95o-92o,85o-82o,75o-72o,62o+,52o+,42o+,32o"
         Action = Fold }
       { StackMin = 0
         StackMax = 12
         History = []
         Range = "22+,A2s+,A2o+,K2s+,K2o+"
         Action = AllIn }
       { StackMin = 12
         StackMax = 25
         History = []
         Range = "J3s-J2s,T3s-T2s,93s-92s,83s-82s,73s-72s,63s-62s,53s-52s,42s+,32s,K2o,Q4o-Q2o,J5o-J2o,T5o-T2o,95o-92o,85o-82o,75o-72o,62o+,52o+,42o+,32o"
         Action = Fold }
       { StackMin = 12
         StackMax = 25
         History = []
         Range = "KTs+,QTs+,J8s+,T8s+,98s,KTo+,Q9o+,J9o+,T8o+,98o"
         Action = Call }
       { StackMin = 12
         StackMax = 25
         History = []
         Range = "99+,A5s+,K9s-K2s,Q9s-Q2s,J7s-J4s,T7s-T4s,97s-94s,84s+,74s+,64s+,54s,A5o+,K9o-K3o,Q8o-Q5o,J8o-J6o,T7o-T6o,97o-96o,86o+,76o"
         Action = MinRaise }
       { StackMin = 12
         StackMax = 25
         History = []
         Range = "88-22,A4s-A2s,A4o-A2o"
         Action = AllIn } 
       { StackMin = 0
         StackMax = 25
         History = [Limp; Raise (0, 3)]
         Range = "Q9o,J9o,T8o+,98o"
         Action = Fold } 
       { StackMin = 0
         StackMax = 25
         History = [Limp; Raise (0, 3)]
         Range = "KTs+,QTs+,J8s+,T8s+,98s,KTo+,QTo+,JTo"
         Action = Call } 
       { StackMin = 0
         StackMax = 25
         History = [Limp; Raise (3, 100)]
         Range = "KTs+,QTs+,J8s+,T8s+,98s,KTo+,QTo+,JTo" // TODO
         Action = Fold } 
       { StackMin = 0
         StackMax = 25
         History = [Raise (0, 100); Raise(0, 3)]
         Range = "K4s-K2s,Q4s-Q2s,J4s,T4s,95s-94s,85s-84s,75s-74s,64s+,54s,K4o-K3o,Q5o,J7o-J6o,T7o-T6o,97o-96o,86o+,76o"
         Action = Fold } 
       { StackMin = 0
         StackMax = 25
         History = [Raise (0, 100); Raise(0, 3)]
         Range = "K9s-K5s,Q9s-Q5s,J7s-J5s,T7s-T5s,97s-96s,86s+,76s,K9o-K5o,Q8o-Q6o,J8o"
         Action = Call } 
       { StackMin = 0
         StackMax = 25
         History = [Raise (0, 100); Raise(0, 3)]
         Range = "99+,AQs+,AJo+"
         Action = MinRaise } 
       { StackMin = 0
         StackMax = 25
         History = [Raise (0, 100); Raise(0, 3)]
         Range = "AJs-A5s,ATo-A5o"
         Action = AllIn } 
       { StackMin = 0
         StackMax = 25
         History = [Limp; Raise (3, 100)]
         Range = "KTs+,QTs+,J8s+,T8s+,98s,KTo+,QTo+,JTo" // TODO
         Action = Fold } 
       { StackMin = 0
         StackMax = 25
         History = [Raise (0, 100); Raise(3, 5)]
         Range = "K4s-K2s,Q4s-Q2s,J6s-J4s,T7s-T4s,97s-94s,85s-84s,75s-74s,64s+,54s,K5o-K3o,Q5o,J6o,T6o,96o,86o,76o"
         Action = Fold } 
       { StackMin = 0
         StackMax = 25
         History = [Raise (0, 100); Raise(3, 5)]
         Range = "K9s-K5s,Q9s-Q5s,J7s,86s+,76s,A7o-A5o,K9o-K6o,Q8o-Q6o,J8o-J7o,T7o,97o,87o"
         Action = Call } 
       { StackMin = 0
         StackMax = 25
         History = [Raise (0, 100); Raise(3, 5)]
         Range = "99+,A5s+,A8o+"
         Action = AllIn } 
         |]
  decideOnRules rules stack history h