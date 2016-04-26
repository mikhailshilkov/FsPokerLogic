namespace Cards

open Hands

module HandValues =

  type PairType =
    | Under
    | Fifth
    | Fourth
    | Third
    | Second of Face
    | Top of Face
    | Over

  type TypeStrength = | Normal | Weak

  type MadeHandValue =
    | Nothing
    | Pair of PairType
    | TwoPair
    | ThreeOfKind
    | Straight of TypeStrength
    | Flush
    | FullHouse of TypeStrength
    | FourOfKind
    | StraightFlush

  type FlushDraw = | Draw | NoFD
  type StraightDraw = | OpenEnded | GutShot | NoSD

  type HandValue = { Made: MadeHandValue; FD: FlushDraw; SD: StraightDraw }

  let streetyFaces amount holes (cards: int list) =
    if List.length cards < amount then None
    else
      let figs = cards |> List.distinct |> Set.ofList
      [1..15-amount-holes]
      |> List.map (fun x -> [x.. x + amount + holes - 1] |> Set.ofList)
      |> List.filter (fun l -> Set.difference l figs |> Set.count <= holes)
      |> List.tryLast
      |> Option.map Set.maxElement

  let streety amount holes (cards: SuitedCard[]) =
    if Array.length cards < amount then None
    else
      cards 
      |> Array.map (fun x -> if x.Face = Ace then [1; faceValue x.Face] else [faceValue x.Face]) 
      |> List.concat 
      |> streetyFaces amount holes

  let isStreety amount holes (cards: SuitedCard[]) =
    streety amount holes cards |> Option.isSome

  let isStraight (cards: SuitedCard[]) =
    isStreety 5 0 cards

  let isOpenEndedStraightDraw hand board =
    let exceptA c = c.Face <> Ace
    let combined = Array.concat [| board; [|hand.Card1; hand.Card2|] |]
    not (isStraight combined)
    && not (isStreety 4 0 (board |> Array.filter exceptA))
    && isStreety 4 0 (combined |> Array.filter exceptA)

  let isGutShot hand board =
    let combined = Array.concat [| board; [|hand.Card1; hand.Card2|] |]
    not (isStraight combined)
    && not (isOpenEndedStraightDraw hand board)
    && not (isStreety 4 1 board)
    && isStreety 4 1 combined

  let isWeakStraight hand board =
    let combined = Array.concat [| board; [|hand.Card1; hand.Card2|] |]
    let our = streety 5 0 combined
    match our with
    | Some s -> 
      let boardFaces = board |> Array.map (fun x -> faceValue x.Face) |> List.ofArray
      [s-4..14]
      |> List.map (fun x -> List.concat [boardFaces; [x]])
      |> List.exists (fun x -> streetyFaces 5 0 x |> Option.filter (fun y -> y > s) |> Option.isSome)
    | _ -> false

  let cardValueGroups cards = cards |> Seq.countBy (fun x -> x.Face) |> Seq.map snd |> Seq.sortDescending 
  let sameSequence a b = Seq.compareWith Operators.compare a b = 0

  let isFullHouse (cards : SuitedCard[]) =
    cards |> cardValueGroups |> Seq.take 2 |> sameSequence (seq [3; 2])

  let isWeakFullHouse hand board =
    hand.Card1.Face = hand.Card2.Face
    && board |> Array.filter (fun x -> x.Face = hand.Card1.Face) |> Array.length = 1
    && board |> Array.filter (fun x -> faceValue x.Face > faceValue hand.Card1.Face) |> Array.length = 4
    && Array.concat [| board; [|hand.Card1; hand.Card2|] |] |> cardValueGroups |> sameSequence (seq [3; 2; 2])


  let isFourOfKind (cards : SuitedCard[]) =
    cards |> cardValueGroups |> Seq.head = 4

  let isFlush (cards : SuitedCard[]) =
    cards |> Seq.countBy (fun x -> x.Suit) |> Seq.map snd |> Seq.exists (fun x -> x >= 5)

  let isFlushDrawWith2 hand board =
    hand.Card1.Suit = hand.Card2.Suit 
    && Array.filter (fun x -> x.Suit = hand.Card1.Suit) board |> Array.length = 2

  let isFlushDraw hand board =
    if hand.Card1.Suit = hand.Card2.Suit then isFlushDrawWith2 hand board 
    else 
      [hand.Card1; hand.Card2]
      |> List.map (fun c -> Array.filter (fun x -> x.Suit = c.Suit) board |> Array.length)
      |> List.exists (fun c -> c = 3)

  let isStraightFlush (cards : SuitedCard[]) =
    let flushSuit = cards |> Seq.countBy (fun x -> x.Suit) |> Seq.filter (fun (x, count) -> count >= 5) |> Seq.map fst |> Seq.tryHead
    match flushSuit with
    | Some s -> cards |> Array.filter (fun x -> x.Suit = s) |> isStraight
    | None -> false

  let isDoublePaired (cards : SuitedCard[]) =
    cards |> cardValueGroups |> Seq.filter (fun x -> x >= 2) |> Seq.length >= 2

  let handValue hand board =
    let combined = Array.concat [| board; [|hand.Card1; hand.Card2|] |]

    let boardFaces =
      board 
      |> Seq.sortByDescending (fun x -> faceValue x.Face)
      |> Seq.map (fun x -> x.Face)
    let boardHighestPair = boardFaces |> Seq.groupBy id |> Seq.filter (fun (_, c) -> Seq.length c >= 2) |> Seq.map (faceValue << fst) |> Seq.tryHead

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
    else if isWeakFullHouse hand board then FullHouse(Weak)
    else if isFullHouse combined then FullHouse(Normal)
    else if isFlush combined then Flush
    else if isStraight combined then 
      if isWeakStraight hand board then Straight(Weak) else Straight(Normal)
    else if Seq.length pairs = 2 && fst pairs.[0] = fst pairs.[1] then ThreeOfKind
    else if pairs |> Seq.filter (fun (x, _) -> faceValue x > defaultArg boardHighestPair 0) |> Seq.length = 2 then TwoPair
    else if isPairedHand then 
      if pairIndex.IsSome then ThreeOfKind
      else
        let boardFaces = boardFaces |> Seq.map faceValue |> Seq.distinct
        let biggerBoardCards = boardFaces |> Seq.filter (fun x -> x > faceValue hand.Card1.Face) |> Seq.length
        match biggerBoardCards with
        | i when i = Seq.length boardFaces -> Pair(Under)
        | 0 -> Pair(Over)
        | 1 -> Pair(Second(Ace))
        | 2 -> Pair(Third)
        | 3 -> Pair(Fourth)
        | 4 -> Pair(Fifth)
        | _ -> failwith "Board too long?"
    else 
      let kicker = if board |> Array.exists (fun x -> x.Face = hand.Card1.Face) then hand.Card2.Face else hand.Card1.Face
      match pairIndex with
      | Some(1) -> Pair(Top(kicker))
      | Some(2) -> Pair(Second(kicker))
      | Some(3) -> Pair(Third)
      | Some(4) -> Pair(Fourth)
      | Some(5) -> Pair(Fifth)
      | _ -> Nothing

  let handValueWithDraws hand board =
    { Made = handValue hand board
      FD = if isFlushDraw hand board then Draw else NoFD
      SD = if isOpenEndedStraightDraw hand board then OpenEnded else if isGutShot hand board then GutShot else NoSD
    }
