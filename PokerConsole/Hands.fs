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
  { Card1 : Face
    Card2 : Face
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
  let v1 = faceValue hand.Card1
  let v2 = faceValue hand.Card2
  if v1 >= v2 then hand
  else { Card1 = hand.Card2; Card2 = hand.Card1; SameSuit = hand.SameSuit }

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

  { Card1 = card1
    Card2 = card2
    SameSuit = sameSuit }
