module ImportTests

open Hands
open Preflop
open Import
open Xunit
open FsCheck
open FsCheck.Xunit
open Excel.Import

let fileNameIP = System.IO.Directory.GetCurrentDirectory() + @"\IPinput.xlsx"
let rulesIP = importExcel (importRulesByStack importRulesIP) fileNameIP |> List.ofSeq
let decideIP = decideOnRules rulesIP

type BigBets25 =
    static member Int() =
        Arb.Default.Decimal()
        |> Arb.filter (fun c -> 1m <= c && c <= 25m)

type BigBets14 =
    static member Int() =
        Arb.Default.Decimal()
        |> Arb.filter (fun c -> 1m <= c && c <= 14m)

[<Property(Arbitrary = [| typeof<BigBets25> |])>]
let ``decide on IP import has value for any stack and hand`` bb h =
  let action = decideIP bb 0 0m [] h
  not (action = None)

let decideOnImportedWithOdds decide handString stack odds openRange history expected =
  let hand = parseHand handString
  let actual = decide stack odds openRange history hand
  Assert.Equal (expected,
    (match actual with
      | None -> "-"
      | Some MinRaise -> "Raise"
      | Some(RaiseX 2.5m) -> "RaiseX2.5"
      | Some(RaiseX 3m) -> "RaiseX3"
      | Some(RaiseBluffX 2.5m) -> "RaiseBluffX2.5"
      | Some(RaiseBluffX 3m) -> "RaiseBluffX3"
      | Some x -> sprintf "%A" x))

let decideOnImported decide handString stack history expected =
  decideOnImportedWithOdds decide handString stack 0 0m history expected

[<Theory>]
[<InlineData("Q4o", 25, "Fold")>]
[<InlineData("KTo", 24, "Call")>]
[<InlineData("AA", 23, "Raise")>]
[<InlineData("22", 22, "AllIn")>]
[<InlineData("T9o", 12, "AllIn")>]
[<InlineData("T7s", 11, "Fold")>]
let ``decide on imported / open`` handString stack expected =
  decideOnImported decideIP handString stack [] expected

[<Theory>]
[<InlineData("98o", 21, 2.5, "Fold")>]
[<InlineData("98s", 20, 2.5, "Call")>]
[<InlineData("KQo", 21, 2.5, "AllIn")>]
[<InlineData("KTs", 19, 3.5, "Fold")>]
[<InlineData("KJs", 20, 3.5, "AllIn")>]
let ``decide on imported / WasLimp raise`` handString stack raiseSize expected =
  decideOnImported decideIP handString stack [WasLimp; WasRaise(raiseSize)] expected

[<Theory>]
[<InlineData("KJo", 25, "Fold")>]
[<InlineData("KQs", 24, "Call")>]
let ``decide on imported / WasLimp allin`` handString stack expected =
  decideOnImported decideIP handString stack [WasLimp; WasRaiseAllIn] expected

[<Theory>]
[<InlineData("95s", 18, 3.2, "Fold")>]
[<InlineData("T5s", 17, 3.2, "Call")>]
[<InlineData("KK", 16, 3.2, "RaiseX2.5")>]
[<InlineData("A5o", 15, 3.2, "AllIn")>]
[<InlineData("T5s", 14, 3.9, "Fold")>]
[<InlineData("T7o", 13, 3.9, "Call")>]
[<InlineData("A6s", 25, 3.9, "AllIn")>]
[<InlineData("K7s", 24, 5.5, "Fold")>]
[<InlineData("76s", 23, 5.5, "Call")>]
[<InlineData("A8o", 22, 5.5, "AllIn")>]
[<InlineData("K9s", 21, 7.1, "Fold")>]
[<InlineData("AKs", 20, 7.1, "AllIn")>]
let ``decide on imported / reraise`` handString stack raiseSize expected =
  decideOnImported decideIP handString stack [WasRaise(2m); WasRaise(raiseSize)] expected

[<Theory>]
[<InlineData("A7s", 19, "Fold")>]
[<InlineData("99", 18, "Call")>]
let ``decide on imported / raise allin`` handString stack expected =
  decideOnImported decideIP handString stack [WasRaise(2m); WasRaiseAllIn] expected

let fileNameOOP = System.IO.Directory.GetCurrentDirectory() + @"\OOPinput.xlsx"
let rulesOOP = importExcel (importRulesByStack importRulesOOP) fileNameOOP |> List.ofSeq
let decideOOP = decideOnRules rulesOOP

[<Theory>]
[<InlineData("QQ", 13, 2.3, "AllIn")>]
[<InlineData("AA", 13, 2.7, "AllIn")>]
let ``decide on imported / 4bet`` handString stack raiseSize expected =
  decideOnImported decideOOP handString stack [WasRaise(raiseSize); WasRaise(2.5m); WasRaise(2m)] expected

[<Theory>]
[<InlineData("JJ", 14, 2.3, "Call")>]
[<InlineData("KK", 14, 2.7, "Call")>]
let ``decide on imported / 4bet allin`` handString stack raiseSize expected =
  decideOnImported decideOOP handString stack [WasRaise(raiseSize); WasRaise(2.5m); WasRaiseAllIn] expected

[<Theory>]
[<InlineData("Q9s", 7, "Check")>]
[<InlineData("JTo", 6, "RaiseX3")>]
[<InlineData("22", 5, "AllIn")>]
let ``decide on imported / WasLimp`` handString stack expected =
  decideOnImported decideOOP handString stack [WasLimp] expected

[<Theory>]
[<InlineData("94s", 4, "Fold")>]
[<InlineData("Q2s", 3, "Call")>]
[<InlineData("88", 2, "RaiseX2.5")>]
[<InlineData("66", 1, "AllIn")>]
let ``decide on imported / raise`` handString stack expected =
  decideOnImported decideOOP handString stack [WasRaise(2.5m)] expected

[<Theory>]
[<InlineData("K6s", 7, "Fold")>]
[<InlineData("K7s", 6, "Call")>]  
let ``decide on imported / allin`` handString stack expected =
  decideOnImported decideOOP handString stack [WasRaiseAllIn] expected

[<Theory>]
[<InlineData("QTo", 5, "Fold")>]
[<InlineData("QJo", 4, "Call")>]
[<InlineData("AJo", 3, "AllIn")>]
let ``decide on imported / WasLimp raise reraise`` handString stack expected =
  decideOnImported decideOOP handString stack [WasLimp; WasRaise(3m); WasRaise(5m)] expected

[<Theory>]
[<InlineData("QTs", 2, "Fold")>]
[<InlineData("KQo", 1, "Call")>]
let ``decide on imported / WasLimp raise allin`` handString stack expected =
  decideOnImported decideOOP handString stack [WasLimp; WasRaise(3m); WasRaiseAllIn] expected

[<Fact>]
let ``decide on imported / allin on 4bet`` () =
  decideOnImported decideOOP "88" 6.66m [WasRaise(2m); WasRaise(4m); WasRaise(6m)] "AllIn"

[<Fact>]
let ``decide on imported / call 4bet allin`` () =
  decideOnImported decideOOP "99" 6.66m [WasRaise(2m); WasRaise(4m); WasRaiseAllIn] "Call"

let fileNameAdvancedOOP = System.IO.Directory.GetCurrentDirectory() + @"\PostflopPART2.xlsx"
let (rulesAdvancedOOPStruct, hudData) = importExcel (fun x -> (importOopAdvanced x, importHudData x)) fileNameAdvancedOOP
let rulesAdvancedOOPLow = List.concat [rulesAdvancedOOPStruct.Always; rulesAdvancedOOPStruct.LimpFoldLow]
let rulesAdvancedOOPBig = List.concat [rulesAdvancedOOPStruct.Always; rulesAdvancedOOPStruct.LimpFoldBig]
let decideAdvancedOOP x = decideOnRules rulesAdvancedOOPLow x
let decideAdvancedOOPBig x = decideOnRules rulesAdvancedOOPBig x
let rulesAllOOPLow = List.concat [rulesOOP; rulesAdvancedOOPStruct.Always; rulesAdvancedOOPStruct.LimpFoldLow]
let decideAllOOP x = decideOnRules rulesAllOOPLow x

[<Property(Arbitrary = [| typeof<BigBets25> |])>]
let ``decide on OOP import has value for any stack and hand`` bb history h =
  let action = decideAllOOP bb 0 0m [history] h
  Assert.NotEqual(None, action)

[<Fact>]
let ``decide on all imported / check back after WasLimp`` () =
  decideOnImported decideAllOOP "K5s" 20m [WasLimp] "Check"

[<Fact>]
let ``decide on all imported / fold crap on pfr`` () =
  decideOnImported decideAllOOP "T2s" 23m [WasRaise 7m] "Fold"

[<Fact>]
let ``decide on all imported / fold weak hand on 4bet`` () =
  decideOnImported decideAllOOP "K9o" 23m [WasRaise 2m; WasRaise 2m; WasRaise 2m] "Fold"

[<Fact>]
let ``decide on all imported / shove vs limp`` () =
  decideOnImported decideAllOOP "99" 23m [WasLimp] "AllIn"

[<Fact>]
let ``decide on PART2 imported / raise FV after WasLimp`` () =
  decideOnImported decideAdvancedOOP "JTs" 20m [WasLimp] "RaiseX3"

[<Fact>]
let ``decide on PART2 imported / raise FB after WasLimp`` () =
  decideOnImported decideAdvancedOOPBig "64s" 20m [WasLimp] "RaiseBluffX3"

[<Fact>]
let ``decide on PART2 imported / all-in after WasLimp`` () =
  decideOnImported decideAdvancedOOP "22" 20m [WasLimp] "AllIn"

[<Fact>]
let ``decide on PART2 imported / all-in after WasLimp/3bet`` () =
  decideOnImported decideAdvancedOOP "TT" 25m [WasLimp; WasRaise(3m); WasRaise(2.5m)] "AllIn"

[<Fact>]
let ``decide on PART2 imported / call all-in 3bet`` () =
  decideOnImported decideAdvancedOOP "TT" 25m [WasLimp; WasRaise(3m); WasRaiseAllIn] "Call"

[<Fact>]
let ``decide on PART2 imported / call WasLimp/3bet`` () =
  decideOnImported decideAdvancedOOP "KTs" 25m [WasLimp; WasRaise(3m); WasRaise(2.5m)] "Call"

[<Theory>]
[<InlineData("K9s", 34)>]
[<InlineData("K8s", 30)>]
[<InlineData("K7s", 25)>]
let ``decide on PART2 imported / call WasLimp/raise FV/3bet with specfied odds`` hand odds =
  decideOnImportedWithOdds decideAdvancedOOP hand 25m odds 0m [WasLimp; WasRaise(3m); WasRaise(2.5m)] "Call"

[<Theory>]
[<InlineData("76s", 30)>]
[<InlineData("T9o", 28)>]
[<InlineData("64s", 25)>]
[<InlineData("43s", 22)>]
let ``decide on PART2 imported / call WasLimp/raise FB/3bet with specfied odds`` hand odds =
  decideOnImportedWithOdds decideAdvancedOOPBig hand 25m odds 0m [WasLimp; WasRaise(3m); WasRaise(2.5m)] "Call"

[<Fact>]
let ``decide on PART2 imported / call pfr`` () =
  decideOnImported decideAdvancedOOP "J3s" 15m [WasRaise(2.5m)] "Call"

[<Fact>]
let ``decide on PART2 imported / 3bet pfr FV`` () =
  decideOnImported decideAdvancedOOP "JJ" 15m [WasRaise(2.5m)] "RaiseX2.5"

[<Fact>]
let ``decide on PART2 imported / 3bet pfr FB`` () =
  decideOnImportedWithOdds decideAdvancedOOP "74s" 25m 0 61m [WasRaise(2m)] "RaiseBluffX2.5"

[<Fact>]
let ``decide on PART2 imported / 5bet ai`` () =
  decideOnImported decideAdvancedOOP "JJ" 25m [WasRaise(2m); WasRaise(2.5m); WasRaise(2m)] "AllIn"

[<Fact>]
let ``decide on PART2 imported / call 4bet ai`` () =
  decideOnImported decideAdvancedOOP "QQ" 25m [WasRaise(2m); WasRaise(2.5m); WasRaiseAllIn] "Call"

[<Fact>]
let ``decide on PART2 imported / 3bet shove based on formula`` () =
  decideOnImportedWithOdds decideAdvancedOOP "Q3s" 21m 0 67m [WasRaise(2m)] "AllIn"

[<Fact>]
let ``decide on PART2 imported Q3s / does not 3bet shove based on formula`` () =
  decideOnImportedWithOdds decideAdvancedOOP "98o" 25m 0 40m [WasRaise(2m)] "Call"

[<Fact>]
let ``decide on PART2 imported / does not 3bet shove based on formula`` () =
  decideOnImportedWithOdds decideAdvancedOOP "97o" 25m 0 40m [WasRaise(2m)] "Call"

[<Fact>]
let ``decide on PART2 imported / fold(check) crap vs limp`` () =
  decideOnImported decideAdvancedOOP "K3s" 18m [WasLimp] "Check"

[<Fact>]
let ``importOopAdvanced imports 3Bet shove ranges correctly`` () =
  let shoveRules = 
    rulesAdvancedOOPLow 
    |> List.filter (fun r -> r.Action = AllIn && match Seq.head r.History with | RaiseFor3BetShove(_, _) -> true | _ -> false)
  Assert.Equal(5 * 37, List.length shoveRules)
  let sampleRule = 
    rulesAdvancedOOPLow
    |> List.filter (fun r -> r.Action = AllIn && r.StackRange = (18, 19) && r.History = seq [RaiseFor3BetShove(24m, 34m)])
    |> List.head
  Assert.Equal("AA-22, A2s+, K4s-K2s, Q6s-Q2s, J6s-J3s, T6s, 96s-95s, 86s-85s, 75s-74s, 64s+, 53s+, 43s, A2o+", sampleRule.Range)

[<Fact>]
let ``importHudData imports player stats from excel`` () =
  let length = List.length hudData
  Assert.Equal(16, length)
  let sample = List.filter (fun x -> x.VillainName = "Peterkoven") hudData |> List.head
  Assert.Equal(31, sample.OpenRaise20_25)
  Assert.Equal(21, sample.OpenRaise16_19)
  Assert.Equal(14, sample.OpenRaise14_15)
  Assert.Equal(59, sample.LimpFold)
