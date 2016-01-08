module Preflop

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

type Suit = 
  | Hearts
  | Diamonds
  | Clubs
  | Spades

type Card = 
  { Face : Face
    Suit : Suit }

type Hand = 
  { High: Card 
    Low: Card}

type Range = Hand seq

let isHandInRange (hand : Hand) (range : Range) : bool = 
  Seq.contains hand range

let anySuit highFace lowFace : Range = 
  [{Face = highFace; Suit = Hearts}; 
  {Face = highFace; Suit = Diamonds}; 
  {Face = highFace; Suit = Clubs}; 
  {Face = highFace; Suit = Spades}] |> seq 

let rangeFromMask anySuit (mask: string) : Range =
  match (mask) with
    | "AA" -> anySuit Ace Ace
    | _ -> failwith "Unknown mask"
  
