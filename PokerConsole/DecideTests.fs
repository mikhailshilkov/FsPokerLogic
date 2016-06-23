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

[<Fact>]
let ``3bet allin for 2.5 raise based on old rules`` () =
  let hb = 20
  let vb = 50
  let bb = 20
  let hs = 500 - hb
  let vs = 500 - vb
  let stack = min (hs + hb) (vs + vb)
  let effectiveStack = decimal stack / decimal bb
  let callSize = min (vb - hb) hs
  let potOdds = (callSize |> decimal) * 100m / (vb + hb + callSize |> decimal) |> ceil |> int
  let fullHand = parseFullHand "AhKh"
  let history = [WasRaise(2.5m)]
  let result = decideOnRules rules effectiveStack potOdds 60m history fullHand
  Assert.Equal(Some AllIn, result)
