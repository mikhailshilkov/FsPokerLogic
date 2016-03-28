module HandsTests

open FsCheck
open FsCheck.Xunit
open Hands

[<Property>]
let ``oneBelow: Any face goes to Two after several calls`` f =
  let rec isAbleToGoToTwo f =
    match f with
    | Two -> true
    | _ -> isAbleToGoToTwo (oneBelow f)

  isAbleToGoToTwo f

[<Property>]
let ``faceToChar and then parseFace give the initial value`` f =
  f = (faceToChar f |> parseFace)

[<Property>]
let ``suitToChar and then parseSuit give the initial value`` f =
  f = (suitToChar f |> parseSuit)

[<Property>]
let ``parseHand parses pairs correctly`` f =
  let c = faceToChar f
  let s = c.ToString() + c.ToString()
  let hand = parseHand s
  hand.Card1 = hand.Card2 && not hand.SameSuit

[<Property>]
let ``parseHand parses non-pairs correctly`` f1 f2 same =
  not (f1 = f2) ==>
    let s = (faceToChar f1).ToString() + (faceToChar f2).ToString() + (if same then "s" else "o")
    let hand = parseHand s
    hand.Card1 = f1 && hand.Card2 = f2 && hand.SameSuit = same

[<Property>]
let ``parseBoard parses boards correctly`` f1 s1 f2 s2 f3 s3 =
  let s = 
    [(f1, s1); (f2, s2); (f3, s3)] 
    |> Seq.map (fun x -> (faceToChar (fst x)).ToString() + (suitToChar (snd x)).ToString())
    |> String.concat ""
  let board = parseBoard s
  board.Length = 3 
  && board.[0].Face = f1 && board.[0].Suit = s1
  && board.[1].Face = f2 && board.[1].Suit = s2
  && board.[2].Face = f3 && board.[2].Suit = s3
