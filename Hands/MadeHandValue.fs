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
    | FullHouse
    | FourOfKind
    | StraightFlush

  let isStraight (cards: SuitedCard list) =
    let figs = cards |> List.map (fun x -> x.Face) |> List.distinct |> List.sort
    let a = Seq.zip figs (Seq.skip 1 figs)
    let b = a |> Seq.filter (fun pair -> faceValue (fst pair) = faceValue (snd pair) + 1)
    let c = b|> Seq.length >= 4
    c

  let cardValueGroups cards = cards |> Seq.countBy (fun x -> x.Face) |> Seq.map snd |> Seq.sortDescending 

  let isFullHouse (cards : SuitedCard list) =
    let sameSequence a b = Seq.compareWith Operators.compare a b = 0
    cards |> cardValueGroups |> Seq.take 2 |> sameSequence (seq [3; 2])

  let isFourOfKind (cards : SuitedCard list) =
    cards |> cardValueGroups |> Seq.head = 4

  let isFlush (cards : SuitedCard list) =
    cards |> Seq.countBy (fun x -> x.Suit) |> Seq.map snd |> Seq.exists (fun x -> x >= 5)

  let isStraightFlush (cards : SuitedCard list) =
    let flushSuit = cards |> Seq.countBy (fun x -> x.Suit) |> Seq.filter (fun (x, count) -> count >= 5) |> Seq.map fst |> Seq.tryHead
    match flushSuit with
    | Some s -> cards |> List.filter (fun x -> x.Suit = s) |> isStraight
    | None -> false

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

    if isStraightFlush combined then StraightFlush
    else if isFourOfKind combined then FourOfKind
    else if isFullHouse combined then FullHouse
    else if isFlush combined then Flush
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