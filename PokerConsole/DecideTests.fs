module DecideTests

open Cards
open Hands
open Actions
open Xunit
open Preflop
open Excel
open Import

let fileNameIP = System.IO.Directory.GetCurrentDirectory() + @"\IPinput.xlsx"
let rulesIP = importRuleFromExcel (importRulesByStack importRulesIP) fileNameIP
let fileNameOOP = System.IO.Directory.GetCurrentDirectory() + @"\OOPinput.xlsx"
let rulesOOP = importRuleFromExcel (importRulesByStack importRulesOOP) fileNameOOP
let fileNameAdvancedOOP = System.IO.Directory.GetCurrentDirectory() + @"\PostflopPART2.xlsx"
let rulesAdvancedOOP = importRuleFromExcel importOopAdvanced fileNameAdvancedOOP
let rules = List.concat [rulesIP; rulesAdvancedOOP.Always; rulesAdvancedOOP.LimpFoldLow; rulesOOP]

let test vb hand openRange =
  let hb = 20
  let bb = 20
  let hs = 500 - hb
  let vs = 500 - vb
  let stack = min (hs + hb) (vs + vb)
  let effectiveStack = decimal stack / decimal bb
  let callSize = min (vb - hb) hs
  let potOdds = (callSize |> decimal) * 100m / (vb + hb + callSize |> decimal) |> ceil |> int
  let fullHand = parseFullHand hand
  let history = [WasRaise(decimal(vb) / decimal(bb))]
  let result = decideOnRules rules effectiveStack potOdds openRange history fullHand
  Assert.Equal(Some AllIn, result)

[<Fact>]
let ``3bet allin for 2.5 raise based on old rules`` () =
  test 50 "AhKh" 60m

[<Fact>]
let ``3bet allin for 2 raise based on 3b shove rules with stats on the edge of two rows`` () =
  test 40 "Ad8c" 48m

[<Fact>]
let ``3bet allin for 5x raise based on old rules`` () =
  test 100 "Js8c" 60m