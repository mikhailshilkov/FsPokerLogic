module Ranges
open Hands

type NonPairRange = 
  { High : Face
    LowMax : Face
    LowMin : Face
    SameSuit : bool }

type PairRange = 
  { High : Face
    Low : Face }

type Range = 
  | Pair of PairRange
  | NonPair of NonPairRange

let isHandInRange range hand = 
  let biggerThan c1 c2 = 
    match (c1, c2) with
    | Ace, _ -> true
    | _, Ace -> false
    | King, _ -> true
    | _, King -> false
    | Queen, _ -> true
    | _, Queen -> false
    | Jack, _ -> true
    | _, Jack -> false
    | Ten, _ -> true
    | _, Ten -> false
    | Nine, _ -> true
    | _, Nine -> false
    | Eight, _ -> true
    | _, Eight -> false
    | Seven, _ -> true
    | _, Seven -> false
    | Six, _ -> true
    | _, Six -> false
    | Five, _ -> true
    | _, Five -> false
    | Four, _ -> true
    | _, Four -> false
    | Three, _ -> true
    | _, Three -> false
    | Two, Two -> true
  match (range) with
  | Pair p -> hand.Face1 = hand.Face2 && biggerThan p.High hand.Face1 && biggerThan hand.Face1 p.Low
  | NonPair r -> 
    r.High = hand.Face1 && biggerThan r.LowMax hand.Face2 && biggerThan hand.Face2 r.LowMin 
    && r.SameSuit = hand.SameSuit

let isHandInRanges ranges hand = Seq.exists (fun r -> isHandInRange r hand) ranges

let parseRange (s : string) = 
  try
    let firstString = if s.Length >= 3 then  s.Substring(0, 3) else s
    let high = parseHand firstString
    if firstString.Length = 2 then
      Pair({ High = high.Face1
             Low = high.Face1 })
    else if s.[2] = '+' then 
      Pair({ High = Ace
             Low = high.Face1 })
    else if s.Length = 3 then 
      let same = s.[2] = 's'
      NonPair({ High = high.Face1
                LowMax = high.Face2
                LowMin = high.Face2
                SameSuit = same })
    else 
      let allUp = s.[3] = '+'
      if high.Face1 = high.Face2 then 
        if allUp then 
          Pair({ High = Ace
                 Low = high.Face1 })
        else 
          let low = parseHand s.[3..4]
          Pair({ High = high.Face1
                 Low = low.Face1 })
      else 
        let same = s.[2] = 's'
        if allUp then 
          NonPair({ High = high.Face1
                    LowMax = (oneBelow high.Face1)
                    LowMin = high.Face2
                    SameSuit = same })
        else 
          let low = parseHand s.[4..6]
          NonPair({ High = high.Face1
                    LowMax = high.Face2
                    LowMin = low.Face2
                    SameSuit = same  })
  with e ->
    printf "Failed to parse the range [%s]" s
    failwith (sprintf "Failed to parse the range [%s]" s)

let parseRanges (s : string) = 
  if s = null || s = "" then Seq.empty
  else Seq.map (fun x -> parseRange x) (s.Replace(" ", "").Split(','))