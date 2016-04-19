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

  let handValue hand board =
    let boardLength = Seq.length board
    let boardFaces =
      board 
      |> Seq.sortByDescending (fun x -> faceValue x.Face)
      |> Seq.map (fun x -> x.Face)

    let pairIndex = 
      boardFaces
      |> Seq.mapi (fun i x -> (x = hand.Card1.Face || x = hand.Card2.Face, i))
      |> Seq.filter (fun (x, i) -> x)
      |> Seq.map snd
      |> Seq.map ((+) 1)
      |> Seq.tryHead

    if hand.Card1.Face = hand.Card2.Face then 
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