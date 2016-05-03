namespace PostFlop

module HandValue =
  open Hands
  open Options
  open Cards.HandValues
  open PostFlop.Decision

  let isSecondPairWithAKKicker handValue =
    match handValue with
    | Pair(Second(King)) | Pair(Second(Ace)) -> true
    | _ -> false

  let isSingleCardFlushDrawWithAKQKicker (hand: SuitedHand) (board: Board) =
    [hand.Card1; hand.Card2]
    |> Seq.exists (fun x -> faceValue x.Face >= 12 
                            && Seq.filter (fun y -> y.Suit = x.Suit) board |> Seq.length = 3)

  let is3SuitedCardsOnBoard board = 
    board
    |> Seq.countBy (fun x -> x.Suit)
    |> Seq.map snd
    |> Seq.sort
    |> Seq.last = 3

  let augmentOptions s handValue o =
    let street = street s
    if street = Flop && s.VillainStack = 0 && isSecondPairWithAKKicker handValue then 
      {o with Donk = OnDonk.CallRaisePet } 
    else if street = Turn && o.CheckRaise = OnCheckRaise.Call && is3SuitedCardsOnBoard s.Board && not(isSingleCardFlushDrawWithAKQKicker s.Hand s.Board) then
      {o with CheckRaise = OnCheckRaise.StackOff } 
    else o