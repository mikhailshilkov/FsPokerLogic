module DecideTests

open Cards
open Hands
open Actions
open Xunit
open Preflop
open Excel
open Import

let fileNameIP = System.IO.Directory.GetCurrentDirectory() + @"\IPinput.xlsx"
let rulesIP = importExcel (importRulesByStack importRulesIP) fileNameIP
let fileNameOOP = System.IO.Directory.GetCurrentDirectory() + @"\OOPinput.xlsx"
let rulesOOP = importExcel (importRulesByStack importRulesOOP) fileNameOOP
let fileNameAdvancedOOP = System.IO.Directory.GetCurrentDirectory() + @"\PostflopPART2.xlsx"
let rulesAdvancedOOP = importExcel importOopAdvanced fileNameAdvancedOOP
let rules = List.concat [rulesIP; rulesAdvancedOOP.Always; rulesAdvancedOOP.LimpFoldLow; rulesOOP]

let test s vb hb hand history openRange expected =
  let bb = 20
  let hs = s - hb
  let vs = 1000 - s - vb
  let stack = min (hs + hb) (vs + vb)
  let effectiveStack = decimal stack / decimal bb
  let callSize = min (vb - hb) hs
  let potOdds = (callSize |> decimal) * 100m / (vb + hb + callSize |> decimal) |> ceil |> int
  let fullHand = parseFullHand hand
  let result = decideOnRules rules effectiveStack potOdds openRange history fullHand
  Assert.Equal(Some expected, result)

let testVsPfr vb hand openRange = test 500 vb 20 hand [WasRaise(decimal(vb) / 20m)] openRange AllIn

[<Fact>]
let ``3bet allin for 2.5 raise based on old rules`` () =
  testVsPfr 50 "AhKh" 60m

[<Fact>]
let ``3bet allin for 2 raise based on 3b shove rules with stats on the edge of two rows`` () =
  testVsPfr 40 "Ad8c" 48m

[<Fact>]
let ``3bet allin for 5x raise based on old rules`` () =
  testVsPfr 100 "Js8c" 60m

[<Fact>]
let ``3bet for bluff`` () =
  test 500 40 20 "5c6d" [WasRaise 2m] 78m (RaiseBluffX 2.5m)

[<Fact>]
let ``push AI with 4bb`` () =
  test 80 15 20 "TcTd" [] 0m AllIn

[<Fact>]
let ``call 4b AI`` () =
  test 400 400 100 "TsTh" [WasRaise 3m; WasRaise 5m; WasRaiseAllIn] 0m Call