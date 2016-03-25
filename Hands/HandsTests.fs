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
let ``faceToChar and them parseFace give the initial value`` f =
  f = (faceToChar f |> parseFace)

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
