module MadeHandValueTests

open Xunit
open FsCheck
open FsCheck.Xunit
open Hands
open Cards.MadeHandValue

let test handS flopS expected =
  let hand = parseSuitedHand handS
  let board = parseBoard flopS
  let actual = handValue hand board
  Assert.Equal(expected, actual)


[<Theory>]
[<InlineData("7s2c", "AdKdJhTh8c")>]
let ``Nothing`` handS flopS =
  Nothing |> test handS flopS

[<Theory>]
[<InlineData("5h5c", "KdJhTh8c")>]
let ``Underpair`` handS flopS =
  Pair(Under) |> test handS flopS

[<Theory>]
[<InlineData("AhAc", "KdJhTh8c")>]
let ``Overpair`` handS flopS =
  Pair(Over) |> test handS flopS

[<Theory>]
[<InlineData("Kh5c", "Th8cKdJh")>]
let ``Top pair`` handS flopS =
  Pair(Top) |> test handS flopS

[<Theory>]
[<InlineData("Jc5c", "Th8cKdJh")>]
[<InlineData("QhQc", "KdJhTh8c5d")>]
let ``Second pair`` handS flopS =
  Pair(Second) |> test handS flopS

[<Theory>]
[<InlineData("AcTc", "Th8cKdJh")>]
[<InlineData("JhJc", "KdQhTh8c5d")>]
let ``Third pair`` handS flopS =
  Pair(Third) |> test handS flopS

[<Theory>]
[<InlineData("Ac8c", "Th8cKdJh")>]
[<InlineData("9h9c", "KdJhTh8c5d")>]
let ``Fourth pair`` handS flopS =
  Pair(Fourth) |> test handS flopS

[<Theory>]
[<InlineData("Ac3c", "Th8cKdJh3h")>]
[<InlineData("6h6c", "KdJhTh8c5d")>]
let ``Fifth pair`` handS flopS =
  Pair(Fifth) |> test handS flopS

[<Theory>]
[<InlineData("Kc3c", "Th8cKdJh3h")>]
let ``Two pairs`` handS flopS =
  TwoPair |> test handS flopS

[<Theory>]
[<InlineData("3d3c", "Th8cKdJh3h")>]
[<InlineData("Kd3c", "Th8c3dJh3h")>]
let ``Three of kind`` handS flopS =
  ThreeOfKind |> test handS flopS

[<Theory>]
[<InlineData("2d3c", "4h5c6d")>]
[<InlineData("KdTc", "Jh2cQd9h3h")>]
[<InlineData("KdTc", "Jh3cQd9h3h")>]
let ``Straight`` handS flopS =
  Straight |> test handS flopS

[<Theory>]
[<InlineData("2d3d", "Ad5d6d")>]
[<InlineData("Kd3d", "2c4dJdAc6d")>]
[<InlineData("Kd3c", "2c4cKhAc6c")>]
[<InlineData("Kc3c", "2c4cJcAc6c")>]
let ``Flush`` handS flopS =
  Flush |> test handS flopS

[<Theory>]
[<InlineData("JdTd", "AdJhTs2sTc")>]
[<InlineData("TdTh", "AdJhTs2sJc")>]
let ``Full house`` handS flopS =
  FullHouse |> test handS flopS

[<Theory>]
[<InlineData("TdTh", "AdJhTs2sTc")>]
[<InlineData("TdJd", "AdJhJs2sJc")>]
let ``Four of kind`` handS flopS =
  FourOfKind |> test handS flopS

[<Theory>]
[<InlineData("2d3d", "4d5d6d")>]
[<InlineData("KcTc", "Jc2cQc9c3h")>]
let ``Straight flush`` handS flopS =
  StraightFlush |> test handS flopS