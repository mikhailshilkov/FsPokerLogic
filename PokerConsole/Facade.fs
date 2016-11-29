namespace Preflop
  open Ranges
  open Preflop.Decide
  open Import

  module Facade =
    let decidePreRegwar xlBeavers regs s history () =
      regs 
      |> List.tryFindIndex ((=) s.VillainName)
      |> Option.bind (fun i ->
        match history with
        | [] -> importRegWarIPRanges xlBeavers i |> Some
        | [WasLimp] ->importRegWarOOPLimpRanges xlBeavers i |> Some
        | [WasRaise _] ->importRegWarOOPRaiseRanges xlBeavers i |> Some
        | [WasRaiseAllIn] ->importRegWarOOPAIRanges xlBeavers i |> Some
        | _ -> None)
      |> Option.map (fun f -> 
        let stack = min (s.HeroStack + s.HeroBet) (s.VillainStack + s.VillainBet)
        let effectiveStack = decimal stack / decimal s.BB |> (+) 0.5m |> int
        let ranges, source = f effectiveStack
        let action = 
          if isHandInRanges ranges s.HeroHand then AllIn 
          elif s.VillainBet <= s.HeroBet then Check
          else Fold
        action, source)

    let decidePre xlBeavers rulesBig rulesLow hudData regs s history =
      let apply f = f()
      let rules = [
        decidePreRegwar xlBeavers regs s history;
        decidePreStandard rulesBig rulesLow hudData s history
      ]
      rules |> Seq.choose apply |> Seq.tryHead