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

  type FlushStrength = Nut | NotNut of Face | Board

  type MadeHandValue =
    | Nothing
    | TwoOvercards
    | Pair of PairType
    | TwoPair
    | ThreeOfKind
    | Straight of TypeStrength
    | Flush of FlushStrength
    | FullHouse of TypeStrength
    | FourOfKind
    | StraightFlush

  type FlushDraw = | Draw of Face | NoFD
  type StraightDraw = | OpenEnded | GutShot | NoSD

  type HandValue = { Made: MadeHandValue; FD: FlushDraw; SD: StraightDraw }

  let concat hand board = Array.concat [| board; [|hand.Card1; hand.Card2|] |]

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
    let combined = concat hand board
    not (isStraight combined)
    && not (isStreety 4 0 (board |> Array.filter exceptA))
    && isStreety 4 0 (combined |> Array.filter exceptA)

  let isGutShot hand board =
    let combined = concat hand board
    not (isStraight combined)
    && not (isOpenEndedStraightDraw hand board)
    && not (isStreety 4 1 board)
    && isStreety 4 1 combined

  let isWeakStraight hand board =
    let combined = concat hand board
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

  // If a better full house can be made with just 1 hole card
  let isWeakFullHouse hand board =
    let fullHouseValue cards =
      let groups = cards |> Array.countBy id |> Array.sortByDescending snd |> Array.take 2
      match groups with
      | [|(h, 3); (l, 2)|] -> h * 100 + l
      | _ -> 0
    let combined = concat hand board |> Array.map (fun x -> faceValue x.Face)
    let our = fullHouseValue combined
    let faces = board |> Array.map (fun x -> faceValue x.Face)
    our > 0 
    && [2..14] |> Seq.map (fun x -> Array.append faces [|x|]) |> Seq.exists (fun x -> fullHouseValue x > our)

  let isFourOfKind (cards : SuitedCard[]) =
    cards |> cardValueGroups |> Seq.head = 4

  let flushSuit cards = 
    cards |> Seq.countBy (fun x -> x.Suit) |> Seq.filter (fun (x, count) -> count >= 5) |> Seq.map fst |> Seq.tryHead

  let isFlush (cards : SuitedCard[]) =
    cards |> Seq.countBy (fun x -> x.Suit) |> Seq.map snd |> Seq.exists (fun x -> x >= 5)

  let flushValue hand board =
    let combined = concat hand board
    match flushSuit combined with
    | Some s -> 
      let highestCard = 
        [hand.Card1; hand.Card2] 
        |> Seq.sortByDescending (fun x -> faceValue x.Face)
        |> Seq.tryFind (fun x -> x.Suit = s)
        |> Option.map (fun x -> x.Face)
      match highestCard with
      | Some h when 
        [faceValue h + 1 .. 14] 
        |> List.forall (fun x -> board |> Array.exists (fun y -> y.Suit = s && faceValue y.Face = x)) 
        -> Nut
      | Some h when 
        board
        |> Array.filter (fun y -> y.Suit = s && faceValue y.Face > faceValue h) 
        |> Array.length = 5
        -> Board
      | Some x -> NotNut x
      | None -> Board
      |> Some
    | None -> None

  let findFlushDrawWith2 hand board =
    if hand.Card1.Suit = hand.Card2.Suit 
       && Array.filter (fun x -> x.Suit = hand.Card1.Suit) board |> Array.length = 2
    then maxFace hand.Card1 hand.Card2 |> Some
    else None

  let isFlushDrawWith2 hand board = match findFlushDrawWith2 hand board with | Some _ -> true | None -> false

  let findFlushDraw hand board =
    if hand.Card1.Suit = hand.Card2.Suit then findFlushDrawWith2 hand board 
    else 
      [hand.Card1; hand.Card2]
      |> List.map (fun c -> (c, Array.filter (fun x -> x.Suit = c.Suit) board |> Array.length))
      |> List.filter (fun (_, c) -> c = 3)
      |> List.map (fun (x, _) -> x.Face)
      |> List.tryHead

  let isFlushDraw hand board = match findFlushDraw hand board with | Some _ -> true | None -> false

  let isStraightFlush (cards : SuitedCard[]) =
    match flushSuit cards with
    | Some s -> cards |> Array.filter (fun x -> x.Suit = s) |> isStraight
    | None -> false

  let monoboardLength (cards : SuitedCard[]) =
    cards |> Seq.countBy (fun x -> x.Suit) |> Seq.map snd |> Seq.sortByDescending id |> Seq.head

  let isPaired (cards : SuitedCard[]) =
    cards |> cardValueGroups |> Seq.exists (fun x -> x = 2)

  let isDoublePaired (cards : SuitedCard[]) =
    cards |> cardValueGroups |> Seq.filter (fun x -> x >= 2) |> Seq.length >= 2

  let overcards hand b = 
    let maxBoard = b |> Array.map (fun x -> faceValue x.Face) |> Array.max
    [hand.Card1; hand.Card2] 
    |> Seq.filter (fun x -> faceValue x.Face > maxBoard) 
    |> Seq.length

  let handValue hand board =
    let combined = concat hand board

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
    let flush = flushValue hand board

    if isStraightFlush combined then StraightFlush
    else if isFourOfKind combined then FourOfKind
    else if isWeakFullHouse hand board then FullHouse(Weak)
    else if isFullHouse combined then FullHouse(Normal)
    else if flush.IsSome then Flush(flush.Value)
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
      | _ -> 
        if overcards hand board >= 2 then TwoOvercards else Nothing

  let handValueWithDraws hand board =
    { Made = handValue hand board
      FD = match findFlushDraw hand board with | Some x -> Draw x | None -> NoFD
      SD = if isOpenEndedStraightDraw hand board then OpenEnded else if isGutShot hand board then GutShot else NoSD
    }

  type BoardTexture = { 
    Streety: bool
    DoublePaired: bool
    Monoboard: int }

  let boardTexture board =
    { Streety = isStreety 4 1 board 
      DoublePaired = isDoublePaired board
      Monoboard = monoboardLength board }
