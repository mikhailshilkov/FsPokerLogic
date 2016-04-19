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