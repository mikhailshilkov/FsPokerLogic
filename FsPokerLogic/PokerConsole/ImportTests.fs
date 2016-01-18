module ImportTests

open Hands
open Preflop
open Import
open Xunit
open FsCheck
open FsCheck.Xunit

let fileNameIP = System.IO.Directory.GetCurrentDirectory() + @"\IPinput.xlsx"
let rules = importRuleFromExcel importRulesIP fileNameIP |> List.ofSeq
let decide = decideOnRules rules

type BigBets =
    static member Int() =
        Arb.Default.Decimal()
        |> Arb.filter (fun c -> 1m <= c && c <= 25m)

[<Property(Arbitrary = [| typeof<BigBets> |])>]
let ``decide on IP import has value for any stack and hand`` bb h =
  let action = decide bb [] h
  not (action = None)

[<Fact>]
let ``decide on 12 BB / AllIn`` () =
  let hand = parseHand "T9o"
  let action = decide 12m [] hand
  not (action = None)
