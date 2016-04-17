module ActionsTests

open Cards
open Actions
open Xunit

[<Theory>]
[<InlineData(60, 500, 150)>]
[<InlineData(67, 400, 165)>]
[<InlineData(59, 300, 145)>]
[<InlineData(53, 200, 130)>]
let ``raiseSize returns correct raise amount`` villainBet stack expected =
  let actual = raiseSize 2.5m villainBet stack
  match actual with
  | RaiseToAmount x -> Assert.Equal (expected, x)
  | _ -> failwith "Raise expected"

[<Theory>]
[<InlineData(60, 200)>]
[<InlineData(120, 425)>]
let ``raiseSize returns all in if raise amount more than 70% of stack`` villainBet stack =
  let actual = raiseSize 2.5m villainBet stack
  Assert.Equal(Action.AllIn, actual)