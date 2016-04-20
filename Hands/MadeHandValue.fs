namespace Cards

open Hands

module MadeHandValue =

  type PairType =
    | Under
    | Fifth
    | Fourth
    | Third
    | Second
    | Top
    | Over

  type HandValue =
    | Nothing
    | Pair of PairType
    | TwoPair
    | ThreeOfKind
    | Straight
    | Flush

  let isStraight (cards: SuitedCard list) =
    let figs = cards |> List.map (fun x -> x.Face) |> List.distinct |> List.sort
    let a = Seq.zip figs (Seq.skip 1 figs)
    let b = a |> Seq.filter (fun pair -> faceValue (fst pair) = faceValue (snd pair) + 1)
    let c = b|> Seq.length >= 4
    c

  let isFlush (cards : SuitedCard list) =
    cards |> Seq.countBy (fun x -> x.Suit) |> Seq.map snd |> Seq.exists (fun x -> x >= 5)

  let handValue hand board =
    let combined = List.concat [List.ofArray board; [hand.Card1; hand.Card2]]

    let boardLength = Seq.length board
    let boardFaces =
      board 
      |> Seq.sortByDescending (fun x -> faceValue x.Face)
      |> Seq.map (fun x -> x.Face)

    let pairs = 
      boardFaces
      |> Seq.mapi (fun i x -> (x, i))
      |> Seq.filter (fun (x, i) -> x = hand.Card1.Face || x = hand.Card2.Face)
      |> List.ofSeq

    let pairIndex = 
      pairs 
      |> Seq.map snd
      |> Seq.map ((+) 1)
      |> Seq.tryHead

    let isPairedHand = hand.Card1.Face = hand.Card2.Face

    if isFlush combined then Flush
    else if isStraight combined then Straight
    else if Seq.length pairs = 2 then 
      if fst pairs.[0] = fst pairs.[1] then ThreeOfKind else TwoPair
    else if isPairedHand then 
      if pairIndex.IsSome then ThreeOfKind
      else
        let biggerBoardCards = boardFaces |> Seq.filter (fun x -> faceValue x > faceValue hand.Card1.Face) |> Seq.length
        match biggerBoardCards with
        | i when i = boardLength -> Pair(Under)
        | 0 -> Pair(Over)
        | 1 -> Pair(Second)
        | 2 -> Pair(Third)
        | 3 -> Pair(Fourth)
        | 4 -> Pair(Fifth)
        | _ -> failwith "Board too long?"
    else 
      match pairIndex with
      | Some(1) -> Pair(Top)
      | Some(2) -> Pair(Second)
      | Some(3) -> Pair(Third)
      | Some(4) -> Pair(Fourth)
      | Some(5) -> Pair(Fifth)
      | _ -> Nothing