namespace PostFlop

module HandValue =
  open Hands
  open Options
  open PostFlop.Decision

  let isSecondPairWithAKKicker (hand: SuitedHand) (board: Board) =
    let handArray = [|hand.Card1; hand.Card2|]
    let secondBoardFace = board |> Array.map (fun x -> x.Face) |> Seq.distinct |> Array.ofSeq |> Array.sortBy (fun x -> faceValue x) |> Array.rev |> Seq.item 1
    let isPairedWithSecondBoardFace = handArray |> Array.exists (fun x -> x.Face = secondBoardFace)
    let hasAK = handArray |> Array.exists (fun x -> x.Face = Ace || x.Face = King)
    isPairedWithSecondBoardFace && hasAK && faceValue secondBoardFace <= 12

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

  let augmentOptions s o =
    let street = street s
    if street = Flop && s.VillainStack = 0 && isSecondPairWithAKKicker s.Hand s.Board then 
      {o with Donk = OnDonk.CallRaisePet } 
    else if street = Turn && o.CheckRaise = OnCheckRaise.Call && is3SuitedCardsOnBoard s.Board && not(isSingleCardFlushDrawWithAKQKicker s.Hand s.Board) then
      {o with CheckRaise = OnCheckRaise.StackOff } 
    else o