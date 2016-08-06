module Hands

type Face = 
  | Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two

type Hand = 
  { Face1 : Face
    Face2 : Face
    SameSuit : bool }

let faceValue face = 
  match face with
  | Ace -> 14
  | King -> 13
  | Queen -> 12
  | Jack -> 11
  | Ten -> 10
  | Nine -> 9
  | Eight -> 8
  | Seven -> 7
  | Six -> 6
  | Five -> 5
  | Four -> 4
  | Three -> 3
  | Two -> 2

let normalize hand =
  let v1 = faceValue hand.Face1
  let v2 = faceValue hand.Face2
  if v1 >= v2 then hand
  else { Face1 = hand.Face2; Face2 = hand.Face1; SameSuit = hand.SameSuit }

let oneBelow face = 
  match face with
  | Ace -> King
  | King -> Queen
  | Queen -> Jack
  | Jack -> Ten
  | Ten -> Nine
  | Nine -> Eight
  | Eight -> Seven
  | Seven -> Six
  | Six -> Five
  | Five -> Four
  | Four -> Three
  | Three -> Two
  | Two -> failwith "Nothing below 2"

let faceToChar f = 
  match (f) with
  | Ace -> 'A'
  | King -> 'K'
  | Queen -> 'Q'
  | Jack -> 'J'
  | Ten -> 'T'
  | Nine -> '9'
  | Eight -> '8'
  | Seven -> '7'
  | Six -> '6'
  | Five -> '5'
  | Four -> '4'
  | Three -> '3'
  | Two -> '2'

let parseFace s = 
  match (s) with
  | 'A' -> Ace
  | 'a' -> Ace
  | 'K' -> King
  | 'k' -> King
  | 'Q' -> Queen
  | 'q' -> Queen
  | 'J' -> Jack
  | 'j' -> Jack
  | 'T' -> Ten
  | 't' -> Ten
  | '9' -> Nine
  | '8' -> Eight
  | '7' -> Seven
  | '6' -> Six
  | '5' -> Five
  | '4' -> Four
  | '3' -> Three
  | '2' -> Two
  | f -> failwith ("Unknown face value" + f.ToString())

let parseHand (s : string) = 
  let card1 = parseFace s.[0]
  let card2 = parseFace s.[1]
  
  let sameSuit = 
    if card1 = card2 then false
    else s.[2] = 's'

  { Face1 = card1
    Face2 = card2
    SameSuit = sameSuit }

let parseFullHand (s : string) =
  let card1 = parseFace s.[0]
  let card2 = parseFace s.[2]
  
  let sameSuit = 
    if card1 = card2 then false
    else s.[1] = s.[3]

  { Face1 = card1
    Face2 = card2
    SameSuit = sameSuit }

type Suit = | Hearts | Diamonds | Clubs | Spades

let parseSuit s = 
  match s with
  | 's' -> Spades
  | 'c' -> Clubs
  | 'd' -> Diamonds
  | 'h' -> Hearts
  | f -> failwith ("Unknown suit" + f.ToString())

let suitToChar s = 
  match s with
  | Spades -> 's'
  | Clubs -> 'c'
  | Diamonds -> 'd'
  | Hearts -> 'h'

type SuitedCard = { Face: Face; Suit: Suit }

type SuitedHand = { Card1: SuitedCard; Card2: SuitedCard }

let parseSuitedHand (s : string) =
  { Card1 = { Face = parseFace s.[0]; Suit = parseSuit s.[1] }
    Card2 = { Face = parseFace s.[2]; Suit = parseSuit s.[3] } 
  }

let toHand suited =
  let ordered = 
    [suited.Card1; suited.Card2]
    |> List.map (fun x -> x.Face)
    |> List.sortByDescending faceValue
  { Face1 = ordered.[0]; Face2 = ordered.[1]; SameSuit = suited.Card1.Suit = suited.Card2.Suit }

let maxFace cards = cards |> Seq.map (fun x -> x.Face) |> Seq.maxBy faceValue
let minFace cards = cards |> Seq.map (fun x -> x.Face) |> Seq.minBy faceValue

type Board = SuitedCard[]

let chunkBySize n s =
    seq {
        let r = ResizeArray<_>()
        for x in s do
            r.Add(x)
            if r.Count = n then
                yield r.ToArray()
                r.Clear()
        if r.Count <> 0 then
            yield r.ToArray()
    }

let parseBoard (s: string) : Board =
  s.ToCharArray()
  |> chunkBySize 2 // Seq.chunkBySize only works in F# 4
  |> Seq.map (fun x -> { Face = parseFace x.[0]; Suit = parseSuit x.[1] })
  |> Array.ofSeq

let canBeFlushDraw flop =
  Seq.countBy (fun x -> x.Suit) flop |> Seq.length = 2

let isMonoboard flop =
  Seq.countBy (fun x -> x.Suit) flop |> Seq.length = 1
