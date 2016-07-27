module HandValuesTests

open Xunit
open FsCheck
open FsCheck.Xunit
open Hands
open Cards.HandValues

let test handS flopS expected =
  let hand = parseSuitedHand handS
  let board = parseBoard flopS
  let actual = handValue hand board
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("AcKc", "QcJcTs", true)>]
[<InlineData("AcKc", "QcTs8c", true)>]
[<InlineData("AcKc", "Ts8c2c", true)>]
[<InlineData("AcKc", "Tc8c2c", false)>]
[<InlineData("AcKc", "Tc8s2s", false)>]
[<InlineData("AcKs", "Ts8s2s", false)>]
[<InlineData("AcKs", "Ts8d2h", false)>]
let ``isFlushDrawWith2 returns true for flush draw with 2 hole cards`` handS flopS expected =
  let hand = parseSuitedHand handS
  let board = parseBoard flopS
  let actual = isFlushDrawWith2 hand board
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("AcKc", "QcTs8c", 'A', 'A')>]
[<InlineData("Jc2c", "Ts8c2sJc", 'J', 'J')>]
[<InlineData("AcKs", "Ts8s2sJd", 'K', '-')>]
[<InlineData("QcKs", "Ts8c2cJc", 'Q', '-')>]
[<InlineData("Jc9c", "Tc8c2c", '-', '-')>]
[<InlineData("AcKc", "Tc8s2s", '-', '-')>]
[<InlineData("AcKs", "Ts8d2h", '-', '-')>]
let ``handValueWithDraws returns FD face value for flush draw`` handS flopS expectedS expectedS2 =
  let hand = parseSuitedHand handS
  let board = parseBoard flopS
  let actual = handValueWithDraws hand board
  let expected = if expectedS = '-' then NoFD else (parseFace expectedS |> Draw)
  Assert.Equal(expected, actual.FD)
  let expected2 = if expectedS2 = '-' then NoFD else (parseFace expectedS2 |> Draw)
  Assert.Equal(expected2, actual.FD2)

[<Theory>]
[<InlineData("QcKs", "Th8s2sJd", true)>]
[<InlineData("Ac9s", "Ts8c2cJc", true)>]
[<InlineData("AcKh", "QcJs8d", false)>]
[<InlineData("AcKh", "2c3s4d", false)>]
[<InlineData("2c3s", "Ts9cQcJc", false)>]
let ``isOpenEndedStraightDraw returns true for OESD`` handS flopS expected =
  let hand = parseSuitedHand handS
  let board = parseBoard flopS
  let actual = isOpenEndedStraightDraw hand board
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("QcKs", "Th3s9s4d", true)>]
[<InlineData("QcAs", "Th3s9s8d", true)>]
[<InlineData("AcKh", "QcJs8d", true)>]
[<InlineData("AcKh", "2c3s4d", true)>]
[<InlineData("QcKs", "Th3s9sJd", false)>]
[<InlineData("Ac9s", "Ts8c2cJc", false)>]
[<InlineData("2c3s", "Ts8cQcJc", false)>]
let ``isGutShot returns true for GS`` handS flopS expected =
  let hand = parseSuitedHand handS
  let board = parseBoard flopS
  let actual = isGutShot hand board
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("TsJdTcAc", true)>]
[<InlineData("Ts8cQcJc", false)>]
[<InlineData("TsTcTdJc", false)>]
let ``isPaired returns true for boards with a pair`` flopS expected =
  let board = parseBoard flopS
  let actual = isPaired board
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("KsJd", true)>]
[<InlineData("2sKs", true)>]
[<InlineData("QcJc", false)>]
[<InlineData("KdKc", false)>]
[<InlineData("AdKc", false)>]
let ``isXHigh returns true for K-high hand`` s expected =
  let hand = parseSuitedHand s
  let actual = isXHigh King hand
  Assert.Equal(expected, actual)  

[<Theory>]
[<InlineData("TsJdTcAc", true)>]
[<InlineData("TsTcTdJc", true)>]
[<InlineData("Ts8cQcJc", false)>]
[<InlineData("", false)>]
[<InlineData("Ks", false)>]
let ``isLastBoardCardOvercard returns true for boards with last card being overcard for previous cards`` flopS expected =
  let board = parseBoard flopS
  let actual = isLastBoardCardOvercard board
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("TsJdTcAcJc", true)>]
[<InlineData("TsJdTcJhJc", true)>]
[<InlineData("Ts8cQcJc", false)>]
[<InlineData("Ts8cTcJc", false)>]
[<InlineData("Ts8cTcTd", false)>]
[<InlineData("TsThTcTd", false)>]
let ``isDoublePaired returns true for boards with two pairs`` flopS expected =
  let board = parseBoard flopS
  let actual = isDoublePaired board
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("As5c", "6s7d8c9c", true)>]
[<InlineData("4s5c", "6s7d8cTc", true)>]
[<InlineData("As2c", "Js3d4c5c7d", true)>]
[<InlineData("AsTc", "6s7d8c9c", false)>]
[<InlineData("2sTc", "AsKdQcJc3c", false)>]
let ``isWeakStraight returns true for weak straights`` handS flopS expected =
  let hand = parseSuitedHand handS
  let board = parseBoard flopS
  let actual = isWeakStraight hand board
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("TsJs6sAsQs", 5)>]
[<InlineData("TsJs6sJcQs", 4)>]
[<InlineData("TsJs6cJcQs", 3)>]
[<InlineData("TsJd6cJcQs", 2)>]
let ``monoboardLength returns the length of longest suit`` flopS expected =
  let board = parseBoard flopS
  let actual = monoboardLength board
  Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("JsTc", "9d6h5d4c")>]
[<InlineData("AsQc", "Jd6h4d4c")>]
let ``TwoOvercards`` handS flopS =
  TwoOvercards |> test handS flopS

[<Theory>]
[<InlineData("7s2c", "AdKdJhTh8c")>]
[<InlineData("AsKc", "7d2dJhTh8c")>]
let ``Nothing`` handS flopS =
  Nothing |> test handS flopS

[<Theory>]
[<InlineData("5h5c", "KdJhTh8c")>]
[<InlineData("8h8c", "Qd9h9hKc")>]
let ``Underpair`` handS flopS =
  Pair(Under) |> test handS flopS

[<Theory>]
[<InlineData("AhAc", "KdJhTh8c")>]
let ``Overpair`` handS flopS =
  Pair(Over) |> test handS flopS

[<Theory>]
[<InlineData("Kh5c", "Th8cKdJh")>]
[<InlineData("7h5c", "7d6c6d5h")>]
let ``Top pair`` handS flopS =
  Pair(Top(Five)) |> test handS flopS

[<Theory>]
[<InlineData("Jc5c", "Th8cKdJh", "5")>]
[<InlineData("QhQc", "KdJhTh8c5d", "A")>]
[<InlineData("QhQc", "KdJhKh8c5d", "A")>]
[<InlineData("Ts7c", "Ac7sAd", "T")>]
[<InlineData("9s8c", "Kc9s8d6s6h", "A")>] 
[<InlineData("Js8c", "KcJs9d9s8h", "8")>] 
[<InlineData("8s7c", "KcKs8s7h", "7")>] 
let ``Second pair`` handS flopS kicker =
  Pair(Second(parseFace kicker)) |> test handS flopS

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
[<InlineData("Qc9c", "QhJc9d4h4h")>]
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
[<InlineData("2d3c", "4h5cAd")>]
let ``Straight normal (not weak)`` handS flopS =
  Straight(Normal) |> test handS flopS

[<Theory>]
[<InlineData("3d4c", "5h6c7d9dTh")>]
[<InlineData("3d6c", "5h7c8d9dTh")>]
[<InlineData("Td6c", "Ah7c8d9dTh")>]
let ``Straight weak`` handS flopS =
  Straight(Weak) |> test handS flopS

[<Theory>]
[<InlineData("Ad3d", "2c4dJdKd6c")>]
[<InlineData("Kd3d", "2c4dJdAd6c")>]
[<InlineData("Jd3c", "2cKdQdAd6d")>]
let ``Flush Nut`` handS flopS =
  Flush(Nut) |> test handS flopS

[<Theory>]
[<InlineData("Kd3d", "2c4dJdAc6d")>]
[<InlineData("Kc3c", "2c4cJcQc6c")>]
let ``Flush K`` handS flopS =
  Flush(NotNut King) |> test handS flopS

[<Theory>]
[<InlineData("2d3d", "Ad5d6d")>]
[<InlineData("Kd3c", "2c4cKhAc6c")>]
let ``Flush 3`` handS flopS =
  Flush(NotNut Three) |> test handS flopS

[<Theory>]
[<InlineData("2d3d", "Ad5d6dKdTd")>]
[<InlineData("Ac5d", "Ad7d6dKdTd")>]
[<InlineData("Ac5c", "Ad7d6dKdTd")>]
let ``Flush Board`` handS flopS =
  Flush(Board) |> test handS flopS

[<Theory>]
[<InlineData("JdTd", "AdJhTs2sTc")>]
[<InlineData("TdTh", "AdJhTs2sJc")>]
let ``Full house normal`` handS flopS =
  FullHouse(Normal) |> test handS flopS

[<Theory>]
[<InlineData("JdJh", "JsQsQsKcKd")>]
[<InlineData("7d7h", "Td9h7s9sTc")>]
[<InlineData("Qd4h", "4d4c8s6s6c")>]
[<InlineData("Jd2h", "JhJcQsQc")>]
[<InlineData("8d8h", "7h7c7dKs")>]
[<InlineData("Ad9h", "4h4c4dJs9d")>]
let ``Full house weak`` handS flopS =
  FullHouse(Weak) |> test handS flopS

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