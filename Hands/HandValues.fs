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

  type TypeStrength = | Normal | Weak | OnBoard

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

  type FlushDraw = | Draw of FlushStrength | NoFD
  type StraightDraw = | OpenEnded | GutShot | NoSD

  type HandValue = { Made: MadeHandValue; FD: FlushDraw; FD2: FlushDraw; SD: StraightDraw }

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
    let combined = concat hand board |> List.ofArray |> List.map (fun x -> faceValue x.Face)
    let possibleStraits =
      [1..14]
      |> List.map (fun x -> x :: combined)
      |> List.filter (fun x -> streetyFaces 5 0 x |> Option.isSome)
      |> List.length
    possibleStraits >= 2 && not (isStreety 4 0 board)

  let isGutShot hand board =
    let combined = concat hand board
    not (isStraight combined)
    && not (isOpenEndedStraightDraw hand board)
    && not (isStreety 4 1 board)
    && isStreety 4 1 combined

  let straightStrength hand board =
    let combined = concat hand board
    let our = streety 5 0 combined
    let boardStraight = streety 5 0 board
    match our with
    | x when x = boardStraight -> OnBoard
    | Some s -> 
      let boardFaces = board |> Array.map (fun x -> faceValue x.Face) |> List.ofArray
      let isWeak =
        [s-4..14]
        |> List.map (fun x -> List.concat [boardFaces; [x]])
        |> List.exists (fun x -> streetyFaces 5 0 x |> Option.filter (fun y -> y > s) |> Option.isSome)
      if isWeak then Weak else Normal
    | _ -> Normal

  let cardValueGroups cards = cards |> Seq.countBy (fun x -> x.Face) |> Seq.map snd |> Seq.sortDescending 
  let sameSequence a b = Seq.compareWith Operators.compare a b = 0

  let isFullHouse (cards : SuitedCard[]) =
    cards |> cardValueGroups |> Seq.truncate 2 |> sameSequence (seq [3; 2])

  // If a better full house can be made with just 1 hole card
  let isWeakFullHouse hand board =
    let fullHouseValue cards =
      let groups = cards |> Array.countBy id |> Array.sortByDescending snd |> Array.truncate 2
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

  let flushSuitFlex min max cards = 
    cards |> Seq.countBy (fun x -> x.Suit) |> Seq.filter (fun (x, count) -> count >= min && count <= max) |> Seq.map fst |> Seq.tryHead

  let flushSuit cards = flushSuitFlex 5 7 cards

  let isFlush (cards : SuitedCard[]) =
    cards |> Seq.countBy (fun x -> x.Suit) |> Seq.map snd |> Seq.exists (fun x -> x >= 5)

  let flushValueFlex min max hand board =
    let combined = concat hand board
    match flushSuitFlex min max combined with
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
        |> Array.length = min
        -> Board
      | Some x -> NotNut x
      | None -> Board
      |> Some
    | None -> None

  let flushValue hand board = flushValueFlex 5 7 hand board

  let findFlushDrawWith2 hand board = 
    if hand.Card1.Suit = hand.Card2.Suit 
       && Array.filter (fun x -> x.Suit = hand.Card1.Suit) board |> Array.length = 2
    then flushValueFlex 4 4 hand board
    else None

  let isFlushDrawWith2 hand board = match findFlushDrawWith2 hand board with | Some _ -> true | None -> false

  let findFlushDraw hand board =
    if hand.Card1.Suit = hand.Card2.Suit then findFlushDrawWith2 hand board 
    else
      let withCard1 = flushValueFlex 5 5 { Card1 = hand.Card1; Card2 = hand.Card1 } board
      match withCard1 with
      | Some _ -> withCard1  
      | None -> flushValueFlex 5 5 { Card1 = hand.Card2; Card2 = hand.Card2 } board

  let isFlushDraw hand board = match findFlushDraw hand board with | Some _ -> true | None -> false

  let isStraightFlush (cards : SuitedCard[]) =
    match flushSuit cards with
    | Some s -> cards |> Array.filter (fun x -> x.Suit = s) |> isStraight
    | None -> false

  let monoboardLength = function
    | [||] -> 0
    | c -> c |> Seq.countBy (fun x -> x.Suit) |> Seq.map snd |> Seq.sortByDescending id |> Seq.head

  let isPaired (cards : SuitedCard[]) =
    cards |> cardValueGroups |> Seq.exists (fun x -> x = 2)

  let cardGroupFace count (cards : SuitedCard[]) =
    cards 
    |> Seq.countBy (fun x -> x.Face) 
    |> Seq.sortByDescending snd
    |> Seq.tryHead
    |> Option.filter (fun x -> snd x = count)
    |> Option.map (fun x -> fst x)

  let pairFace (cards : SuitedCard[]) = cardGroupFace 2 cards
  let tripsFace (cards : SuitedCard[]) = cardGroupFace 3 cards

  let anyPairFace (cards : SuitedCard[]) =
    cards 
    |> Seq.countBy (fun x -> x.Face) 
    |> Seq.filter (fun x -> snd x = 2)
    |> Seq.tryHead
    |> Option.map (fun x -> fst x)

  let topPaired (cards : SuitedCard[]) =
    pairFace cards |> Option.filter (fun x -> x = maxFace cards) |> Option.isSome

  let bottomPaired (cards : SuitedCard[]) =
    pairFace cards |> Option.filter (fun x -> x = minFace cards) |> Option.isSome

  let isDoublePaired (cards : SuitedCard[]) =
    cards |> cardValueGroups |> Seq.filter (fun x -> x >= 2) |> Seq.length >= 2

  let isXHigh x hand =
    let values = [hand.Card1; hand.Card2] |> List.map (fun x -> faceValue x.Face) |> List.sortDescending
    let xValue = faceValue x
    match values with
    | y::rem when y = xValue && rem |> List.forall ((>) xValue) -> true
    | _ -> false

  let isLastBoardCardOvercard (board: SuitedCard[]) =
    match Array.rev board |> List.ofArray with
    | [] | [_] -> false
    | last::remaining ->
      let latestCardValue = last.Face |> faceValue
      List.forall (fun x -> faceValue x.Face < latestCardValue) remaining

  let isLastBoardCardSecondCard (board: SuitedCard[]) =
    match Array.rev board |> List.ofArray with
    | [] | [_] -> false
    | last::remaining ->
      let latestCardValue = last.Face |> faceValue
      let values = remaining |> List.map (fun x -> faceValue x.Face) |> List.distinct |> List.sortDescending
      List.length values >= 2 && values.[0] > latestCardValue && values.[1] < latestCardValue

  let isLastBoardCardUndercard (board: SuitedCard[]) =
    match Array.rev board |> List.ofArray with
    | [] | [_] -> false
    | last::remaining ->
      let latestCardValue = last.Face |> faceValue
      List.forall (fun x -> faceValue x.Face > latestCardValue) remaining

  let isLastBoardCardFlushy board = 
    let monoNow = monoboardLength board
    monoNow >= 3 && monoNow > (board |> Array.take (board.Length - 1) |> monoboardLength)

  let maxBoard b = b |> Array.map (fun x -> faceValue x.Face) |> Array.max

  let overcards hand b = 
    [hand.Card1; hand.Card2] 
    |> Seq.filter (fun x -> faceValue x.Face > maxBoard b) 
    |> Seq.length

  let pairIndeces hand board = 
    board 
    |> Seq.map (fun x -> x.Face)
    |> Seq.sortByDescending faceValue
    |> Seq.distinct
    |> Seq.mapi (fun i x -> (x, i + 1))
    |> Seq.filter (fun (x, _) -> x = hand.Card1.Face || x = hand.Card2.Face)
    |> Seq.map snd

  let handValue hand board =
    let combined = concat hand board

    let boardFaces =
      board 
      |> Seq.map (fun x -> x.Face)
      |> Seq.sortByDescending faceValue
    let boardHighestPair = boardFaces |> Seq.groupBy id |> Seq.filter (fun (_, c) -> Seq.length c >= 2) |> Seq.map (faceValue << fst) |> Seq.tryHead

    let pairs = 
      boardFaces
      |> Seq.filter (fun x -> x = hand.Card1.Face || x = hand.Card2.Face)
      |> List.ofSeq

    let pairIndex = 
      boardFaces
      |> Seq.distinct
      |> Seq.mapi (fun i x -> (x, i + 1))
      |> Seq.filter (fun (x, _) -> x = hand.Card1.Face || x = hand.Card2.Face)
      |> Seq.map snd
      |> Seq.tryHead

    let isPairedHand = hand.Card1.Face = hand.Card2.Face
    let flush = flushValue hand board

    if isStraightFlush combined then StraightFlush
    else if isFourOfKind combined then FourOfKind
    else if isFullHouse combined then 
      FullHouse(if isFullHouse board then OnBoard elif isWeakFullHouse hand board then Weak else Normal)
    else if flush.IsSome then Flush(flush.Value)
    else if isStraight combined then 
      Straight(straightStrength hand board)
    else if Seq.length pairs = 2 && pairs.[0] = pairs.[1] then ThreeOfKind
    else if pairs |> Seq.length = 2 && boardHighestPair.IsNone then TwoPair
    else if pairs |> Seq.filter (fun x -> faceValue x > defaultArg boardHighestPair 0) |> Seq.length = 2 then 
      let maxBoardCard = maxBoard board
      if pairs |> Seq.exists (fun x -> faceValue x = maxBoardCard) then TwoPair else Pair(Second(Ace))
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
        if overcards hand board >= 2 && board.Length < 5 then TwoOvercards else Nothing

  let handValueWithDraws hand board =
    { Made = handValue hand board
      FD = match findFlushDraw hand board with | Some x -> Draw x | None -> NoFD
      FD2 = match findFlushDrawWith2 hand board with | Some x -> Draw x | None -> NoFD
      SD = if isOpenEndedStraightDraw hand board then OpenEnded else if isGutShot hand board then GutShot else NoSD
    }

  type BoardTexture = { 
    Streety: bool
    DoublePaired: bool
    ThreeOfKind: bool
    FourOfKind: bool
    Monoboard: int }

  let boardTexture board =
    { Streety = isStreety 4 1 board 
      DoublePaired = isDoublePaired board
      ThreeOfKind = cardValueGroups board |> Seq.head = 3
      FourOfKind = cardValueGroups board |> Seq.head = 4
      Monoboard = monoboardLength board }

  open System
  let inputString = "2.23"
  let (success, result) = Double.TryParse inputString