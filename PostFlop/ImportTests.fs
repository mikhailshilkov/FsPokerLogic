namespace PostFlop

module ImportTests =

  open Microsoft.FSharp.Core
  open Import
  open Hands
  open Decision
  open Cards.Actions
  open Cards.HandValues
  open Options
  open Xunit
  open Microsoft.Office.Interop.Excel
  open System.Runtime.InteropServices
  open Excel.Import

  let defaultTexture = { Streety = false; DoublePaired = false; ThreeOfKind = false; FourOfKind = false; Monoboard = 2 }
  let defaultOopOptions = { First = Check; Then = Fold; Special = []; Scenario = null; SpecialScenario = null }
  let defaultIpOptions = { CbetFactor = Never; CheckRaise = OnCheckRaise.Call; Donk = OnDonk.Fold; DonkRaise = OnDonkRaise.Undefined }  
  let defaultFlop = { Hand = parseSuitedHand "7s2c"; Board = parseBoard "KdJs6c"; Pot = 80; VillainStack = 440; HeroStack = 480; VillainBet = 0; HeroBet = 0; BB = 20 }
  let defaultTurn = { Hand = parseSuitedHand "7s2c"; Board = parseBoard "KdJs6c2d"; Pot = 280; VillainStack = 340; HeroStack = 380; VillainBet = 100; HeroBet = 0; BB = 20 }
  let defaultRiver = { Hand = parseSuitedHand "7s2c"; Board = parseBoard "KdJs6c2d9d"; Pot = 280; VillainStack = 340; HeroStack = 380; VillainBet = 100; HeroBet = 0; BB = 20 }
  let defaultHistory = [notMotivated PreFlop 20 (Action.RaiseToAmount 40); notMotivated Flop 0 (Action.RaiseToAmount 50)]
  let defaultMade = { Made = Nothing; FD = NoFD; FD2 = NoFD; SD = NoSD }

  [<Theory>]
  [<InlineData("2s2c2d", 0)>]
  [<InlineData("2s3c3d", 13)>]
  [<InlineData("2s4c4d", 25)>]
  [<InlineData("6s7c2d", 47)>]
  [<InlineData("2sAcAd", 90)>]
  [<InlineData("3s3c3d", 91)>]
  [<InlineData("3s6cTd", 128)>]
  [<InlineData("4s4c4d", 169)>]
  [<InlineData("AsTc6d", 324)>]
  [<InlineData("KsJcQd", 440)>]
  [<InlineData("AsAcAd", 454)>]
  let ``rowIndex for 222`` h expected =
    let hand = h |> parseBoard
    let actual = rowIndex hand
    Assert.Equal(expected, actual)

  [<Theory>]
  [<InlineData("3c4s5d", false, 14, 14, 15)>]
  [<InlineData("3c4s5dQd", false, 14, 14, 15)>]
  [<InlineData("3c4s5dQd", true, 8, 8, 8)>]
  let ``importOptions returns correct options for a sample cell`` boardString limpedPot ifPre1 ifPre2 ifPre3 =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopIP.xlsx"
    use xl = useExcel fileName
    let board = parseBoard boardString
    let hand = { Card1 = { Face = Ace; Suit = Clubs; }; Card2 = { Face = Two; Suit = Spades } }
    let actual = importOptions xl.Workbook hand board limpedPot
    let expected = { 
      CbetFactor = Always 50m
      CheckRaise = OnCheckRaise.CallEQ 1
      Donk = OnDonk.CallEQ 17
      DonkFlashDraw = Some OnDonk.ForValueStackOff
      TurnFVCbetCards = "8,Q"
      TurnFVCbetFactor = OrAllIn { DefaultCBetOr with Factor = 62.5m; IfPreStackLessThan = ifPre1 }
      TurnCheckRaise = OnCheckRaise.StackOff
      TurnFBCbetCards = "T,J,K,A"
      TurnFBCbetFactor = OrAllIn { DefaultCBetOr with Factor = 62.5m; IfPreStackLessThan = ifPre2 }
    }
    Assert.Equal(expected, actual)

  let handStrengthFileName = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"

  [<Fact>]
  let ``importTurnDonk returns correct option for a sample cell`` () =
    use xl = useExcel handStrengthFileName
    let (fst, snd, _, source) = importTurnDonk xl.Workbook { Made = Pair(Over); FD = NoFD; FD2 = NoFD; SD = NoSD } defaultTexture defaultTurn defaultHistory
    Assert.Equal(OnDonk.Call, fst)
    Assert.Equal(OnDonkRaise.Undefined, snd)
    Assert.Equal("HandStrength -> vill xc F + dbT or dbF + dbT -> F18", source)

  [<Fact>]
  let ``importTurnDonk returns correct option when special conditions apply`` () =
    use xl = useExcel handStrengthFileName
    let special = { defaultTexture with Streety = true }
    let (fst, _, _, _) = importTurnDonk xl.Workbook { Made = Pair(Second Ten); FD = NoFD; FD2 = NoFD; SD = NoSD } special defaultTurn defaultHistory
    Assert.Equal(OnDonk.CallEQ 18, fst)

  [<Fact>]
  let ``importTurnDonk returns correct option on monobooard`` () =
    use xl = useExcel handStrengthFileName
    let special = { defaultTexture with Monoboard = 4 }
    let (fst, snd, _, source) = importTurnDonk xl.Workbook { Made = Flush(NotNut Jack); FD = NoFD; FD2 = NoFD; SD = NoSD } special defaultTurn defaultHistory
    Assert.Equal(OnDonk.RaiseX 260, fst)
    Assert.Equal(OnDonkRaise.CallEQ 10, snd)
    Assert.Equal("HandStrength -> vill xc F + dbT or dbF + dbT -> AG9", source)

  [<Fact>]
  let ``importTurnDonk calculates donk size properly in 3bet pot`` () =
    use xl = useExcel handStrengthFileName
    let s = { defaultTurn with VillainBet = 140; HeroBet = 60; Pot = 280 }
    let h = [notMotivated PreFlop 20 Action.Call; notMotivated Flop 20 Action.Call; notMotivated Turn 20 (RaiseToAmount 60)]
    let (fst, snd, _, source) = importTurnDonk xl.Workbook { Made = Pair(Top Five); FD = NoFD; FD2 = NoFD; SD = GutShot } defaultTexture s h
    Assert.Equal(OnDonk.RaiseGay, fst)
    Assert.Equal(OnDonkRaise.StackOff, snd)
    Assert.Equal("HandStrength -> vill xc F + dbT or dbF + dbT -> C56", source)

  [<Fact>]
  let ``importTurnDonk imports scenario motivation`` () =
    use xl = useExcel handStrengthFileName
    let s = { defaultTurn with VillainBet = 40 }
    let (fst, snd, m, source) = importTurnDonk xl.Workbook { Made = Pair(Fourth); FD = NoFD; FD2 = NoFD; SD = NoSD } defaultTexture s defaultHistory
    Assert.Equal(OnDonk.RaiseGay, fst)
    Assert.Equal(OnDonkRaise.CallEQ 12, snd)
    Assert.Equal(Some(Scenario("r8")), m)
    Assert.Equal("HandStrength -> vill xc F + dbT or dbF + dbT -> C34", source)

  let postflopOOPFileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopOOP.xlsx"

  [<Fact>]
  let ``importOopFlop returns correct options for a sample cell`` () =
    use xl = useExcel postflopOOPFileName
    let actual = importOopFlop xl.Workbook "limp and check" { Made = Pair(Second Ten); FD = NoFD; FD2 = NoFD; SD = NoSD } defaultTexture
    let expected = ({ defaultOopOptions with Then = Raise(2.75m, OopOnCBet.CallEQ 20) }, "limp and check -> B/C13") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importOopFlop returns AI special option`` () =
    use xl = useExcel postflopOOPFileName
    let actual = importOopFlop xl.Workbook "hero call raise pre" { Made = Pair(Second Ten); FD = NoFD; FD2 = NoFD; SD = NoSD } defaultTexture
    let expected = ({ defaultOopOptions with Then = CallEQ 34; Special = [CallEQPlusXvsAI 5] }, "hero call raise pre -> B/C13") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importOopTurn returns correct options for a sample cell`` () =
    use xl = useExcel postflopOOPFileName
    let texture = { defaultTexture with Monoboard = 4 }
    let actual = importOopTurn xl.Workbook "limp and check" { Made = Flush(NotNut Queen); FD = NoFD; FD2 = NoFD; SD = NoSD } texture
    let expected = ({ defaultOopOptions with First = Donk(75m); Then = Call }, "limp and check -> AA/AB21") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importOopTurn returns correct special options for a sample cell`` () =
    use xl = useExcel postflopOOPFileName
    let texture = { defaultTexture with Monoboard = 3; Streety = true }
    let actual = importOopTurn xl.Workbook "limp and check" { Made = TwoPair; FD = Draw(NotNut(King)); FD2 = NoFD; SD = NoSD } texture
    let expected = ({ defaultOopOptions with Then = CallEQ 28 }, "limp and check -> S/T17") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importOopTurn returns correct scenario for a sample cell`` () =
    use xl = useExcel postflopOOPFileName
    let actual = importOopTurn xl.Workbook "hero call raise pre" { defaultMade with SD = OpenEnded } defaultTexture
    let expected = ({ defaultOopOptions with First = Donk 50m; Then = CallEQ 20; Scenario = "r8" }, "hero call raise pre -> K/N32") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importOopTurn returns correct special scenario for a sample cell`` () =
    use xl = useExcel postflopOOPFileName
    let actual = importOopTurn xl.Workbook "hero call raise pre" { defaultMade with SD = GutShot } defaultTexture
    let expected = ({ defaultOopOptions with Then = CallEQ 13; Special = [CheckCheckAndBoardOvercard (Donk 75M, CallEQ 22)]; SpecialScenario = "r9" }, "hero call raise pre -> K/N26") |> Some
    Assert.Equal(expected, actual)

  let testParseTurnDonk s d r =
    let actual = parseTurnDonk s
    Assert.Equal(d, fst actual)
    Assert.Equal(r, snd actual)

  [<Fact>]
  let ``parseTurnDonk call works`` () = testParseTurnDonk "c" OnDonk.Call OnDonkRaise.Undefined

  [<Fact>]
  let ``parseTurnDonk fold works`` () = testParseTurnDonk "f" OnDonk.Fold OnDonkRaise.Undefined

  [<Fact>]
  let ``parseTurnDonk AI works`` () = testParseTurnDonk "AI" OnDonk.AllIn OnDonkRaise.Undefined

  [<Fact>]
  let ``parseTurnDonk rTsdb/18 works`` () = testParseTurnDonk "rTsdb/18" (OnDonk.RaisePreDonkX 110) (OnDonkRaise.CallEQ 18)
  
  [<Fact>]
  let ``parseTurnDonk rTbdb/soT works`` () = testParseTurnDonk "rTbdb/soT" (OnDonk.RaiseX 260) OnDonkRaise.StackOff
  
  [<Fact>]
  let ``parseTurnDonk rTg/20 works`` () = testParseTurnDonk "rTg/20" OnDonk.RaiseGay (OnDonkRaise.CallEQ 20)
  
  [<Fact>]
  let ``parseTurnDonk rTfb/10 works`` () = testParseTurnDonk "rTfb/10" (OnDonk.RaiseX 220) (OnDonkRaise.CallEQ 10)
  
  [<Fact>]
  let ``parseTurnDonk 34 works`` () = testParseTurnDonk "34" (OnDonk.CallEQ 34) OnDonkRaise.Undefined

  [<Fact>]
  let ``parseTurnDonk call^0,6 works`` () = testParseTurnDonk "call^0,6" (OnDonk.RaiseConditional { Size = 2.2m; MinStackPotRatio = 0.6m }) OnDonkRaise.StackOff

  [<Fact>]
  let ``parseTurnDonk F1RR/so works`` () = testParseTurnDonk "F1RR/so" OnDonk.FormulaRaise OnDonkRaise.StackOff

  [<Fact>]
  let ``parseTurnDonk RTV"c/c works`` () = testParseTurnDonk "RTV\"c/c" (OnDonk.RaiseThinValue(OnDonk.Call)) OnDonkRaise.Call

  [<Fact>]
  let ``importOopRiver returns correct options for a sample cell`` () =
    use xl = useExcel postflopOOPFileName
    let actual = importOopRiver xl.Workbook "limp and check" (FullHouse(Normal)) defaultTexture defaultRiver
    let expected = ({ defaultOopOptions with First = Donk(62.5m); Then = StackOff }, "limp and check -> AE/AI21") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importOopRiver returns correct options for a sample cell with new hand value of two pair`` () =
    use xl = useExcel postflopOOPFileName
    let s = { defaultRiver with Hand = parseSuitedHand "Qd4c"; Board = parseBoard "8s9d3hQh3c" }
    let actual = importOopRiver xl.Workbook "hero call raise pre" TwoPair defaultTexture s
    let expected = ({ defaultOopOptions with First = RiverBetValue; Then = StackOff }, "hero call raise pre -> AE/AI42") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importOopRiver returns correct special options for a sample cell`` () =
    use xl = useExcel postflopOOPFileName
    let texture = { defaultTexture with DoublePaired = true }
    let actual = importOopRiver xl.Workbook "limp and check" (Flush(NotNut Eight)) texture defaultRiver
    let expected = ({ defaultOopOptions with First = Donk(50m); Then = Fold }, "limp and check -> AE/AI20") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importOopRiver returns correct scenario for a sample cell`` () =
    use xl = useExcel postflopOOPFileName
    let s = { defaultRiver with Board = parseBoard "8s9d3hQhTc" }
    let actual = importOopRiver xl.Workbook "hero call raise pre" Nothing defaultTexture s
    let expected = ({ defaultOopOptions with Then = CallEQ 5; Scenario = "r8/5" }, "hero call raise pre -> AE/AI25") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importOopRiver returns correct scenario for 3-monoboard on river a sample cell`` () =
    use xl = useExcel postflopOOPFileName
    let s = { defaultRiver with Board = parseBoard "8s9dThQh3h" }
    let texture = { defaultTexture with Monoboard = 3 }
    let actual = importOopRiver xl.Workbook "hero call raise pre" (Pair(Third)) texture s
    let expected = ({ defaultOopOptions with Then = CallEQ 21; Scenario = "r9/15" }, "hero call raise pre -> AF/AI31") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importOopRiver returns correct options on 4-monoboard`` () =
    use xl = useExcel postflopOOPFileName
    let texture = { defaultTexture with Monoboard = 4 }
    let actual = importOopRiver xl.Workbook "limp and check" (FullHouse(Weak)) texture defaultRiver
    let expected = ({ defaultOopOptions with First = Donk(50m); Then = Fold }, "limp and check -> AJ/AK22") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importOopRiver returns correct special options on 4-monoboard`` () =
    use xl = useExcel postflopOOPFileName
    let texture = { defaultTexture with Monoboard = 4; Streety = true }
    let actual = importOopRiver xl.Workbook "hero raise FV vs limp" ThreeOfKind texture defaultRiver
    let expected = (defaultOopOptions, "hero raise FV vs limp -> AJ/AK17") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importOopRiver returns correct options on 4-monoboard with flush`` () =
    use xl = useExcel postflopOOPFileName
    let texture = { defaultTexture with Monoboard = 4 }
    let actual = importOopRiver xl.Workbook "limp and check" (Flush(NotNut King)) texture defaultRiver
    let expected = ({ defaultOopOptions with First = Donk(62.5m); Then = Call }, "limp and check -> AG/AK48") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importOopRiver returns correct options on 5-monoboard with flush`` () =
    use xl = useExcel postflopOOPFileName
    let texture = { defaultTexture with Monoboard = 5 }
    let actual = importOopRiver xl.Workbook "limp and check" (Flush(Board)) texture defaultRiver
    let expected = ({ defaultOopOptions with Then = CallEQ 25 }, "limp and check -> AE54") |> Some
    Assert.Equal(expected, actual)

  let testParseFlopOop s f t =
    let actual = parseOopOption s ""
    let expected = { defaultOopOptions with First = f; Then = t } |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``parseFlopOop ch/25 works`` () = testParseFlopOop "ch/25" Check (CallEQ 25)

  [<Fact>]
  let ``parseFlopOop 62.5%/30 works`` () = testParseFlopOop "62.5%/30" (Donk 62.5m) (CallEQ 30)

  [<Fact>]
  let ``parseFlopOop ch/r/f works`` () = testParseFlopOop "ch/r/f" Check (Raise(2.75m, OopOnCBet.Fold))

  [<Fact>]
  let ``parseFlopOop ch/r/c works`` () = testParseFlopOop "ch/r/c" Check (Raise(2.75m, OopOnCBet.Call))

  [<Fact>]
  let ``parseFlopOop 75%/c works`` () = testParseFlopOop "75%/c" (Donk 75m) Call

  [<Fact>]
  let ``parseFlopOop ch/r/20 works`` () = testParseFlopOop "ch/r/20" Check (Raise(2.75m, OopOnCBet.CallEQ 20))

  [<Fact>]
  let ``parseFlopOop RBS/18 works`` () = testParseFlopOop "RBS/18" RiverBetSizing (CallEQ 18)

  [<Fact>]
  let ``parseFlopOop RBV/15 works`` () = testParseFlopOop "RBV/15" RiverBetValue (CallEQ 15)

  [<Fact>]
  let ``parseFlopOop RBtV/AI works`` () = testParseFlopOop "RBtV/AI" RiverBetThinValue OopOnCBet.AllIn

  [<Fact>]
  let ``parseFlopOop ch/rTbdb/so works`` () = testParseFlopOop "ch/rTbdb/so" Check (Raise(2.6m, OopOnCBet.StackOff))

  [<Fact>]
  let ``parseFlopOop ch/rTfb/34 works`` () = testParseFlopOop "ch/rTfb/34" Check (Raise(2.2m, OopOnCBet.CallEQ 34))

  [<Fact>]
  let ``parseFlopOop ch/rModx2,3/so works`` () = testParseFlopOop "ch/rModx2,3/so" Check (Raise(2.3m, OopOnCBet.StackOff))

  [<Fact>]
  let ``parseFlopOop RBS/20"15 works`` () = testParseFlopOop "RBS/20\"15" RiverBetSizing (CallEQIfRaised(20, 15))

  [<Fact>]
  let ``parseOopOption ch works - mostly used for IP vs check`` () = 
    let actual = parseOopOption "ch" ""
    let expected = { defaultOopOptions with First = Check } |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``parseOopOption 34 works - mostly used for IP vs donk`` () = 
    let actual = parseOopOption "34" ""
    let expected = { defaultOopOptions with Then = CallEQ 34 } |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``parseOopOption ch/25/ovso works`` () = 
    let actual = parseOopOption "ch/25@ovso" ""
    let expected = { defaultOopOptions with First = Check; Then = CallEQ 25; Special = [BoardOvercard(Donk 67m, StackOff)] } |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``parseOopOption 62,5%/so*r9 and Ovso*r8 works`` () = 
    let actual = parseOopOption "62,5%/so*r9" "Ovso*r8"
    let expected = { First = Donk 62.5m; Then = StackOff; Scenario = "r9"; Special = [BoardOvercard(Donk 67m, StackOff)]; SpecialScenario = "r8" } |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``parseOopOption ch/F1RR/20 works`` () = 
    let actual = parseOopOption "ch/F1RR/20" ""
    let expected = { defaultOopOptions with First = Check; Then = FormulaRaise { Or = AllIn; On3Bet = CallEQ 20 } } |> Some
    Assert.Equal(expected, actual)


  [<Fact>]
  let ``parseOopOption F1RR/22"14 works`` () = 
    let actual = parseOopOption "ch/F1RR\"14/22" ""
    let expected = { defaultOopOptions with Then = FormulaRaise { Or = CallEQ 14; On3Bet = CallEQ 22 } } |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``parseOopOption RTV/22"14 works`` () = 
    let actual = parseOopOption "ch/RTV\"c/so" ""
    let expected = { defaultOopOptions with Then = RaiseThinValue { Or = Call; On3Bet = StackOff } } |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``parseOopOption Ch/rTfbFT$100/10  works`` () = 
    let actual = parseOopOption "Ch/rTfbFT$100/10" ""
    let expected = { defaultOopOptions with Then = RaiseConditional { Size = 2.9m; MinStackRemaining = 100; MinStackPotRatio = 0.0m; On3Bet = CallEQ 10 } } |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``parseOopOption Ch/rcombo&0,55/15 works`` () = 
    let actual = parseOopOption "Ch/rcombo&0,55/15" ""
    let expected = { defaultOopOptions with Then = RaiseConditional { Size = 2.2m; MinStackRemaining = 0; MinStackPotRatio = 0.55m; On3Bet = CallEQ 15 } } |> Some
    Assert.Equal(expected, actual)

  let testParseOopSpecialRules s e =
    let actual = parseOopSpecialRules s |> List.head
    Assert.Equal(e, actual)

  [<Fact>]
  let ``parseOopSpecialRules AI#15 works`` () = testParseOopSpecialRules "AI#15" (CallEQPlusXvsAI 15)

  [<Fact>]
  let ``parseOopSpecialRules 6 works`` () = testParseOopSpecialRules "6" (BoardOvercard(Check, Call))

  [<Fact>]
  let ``parseOopSpecialRules Ov works`` () = testParseOopSpecialRules "Ov" (BoardOvercard(OopDonk.AllIn, AllIn))

  [<Fact>]
  let ``parseOopSpecialRules ov AI works`` () = testParseOopSpecialRules "ov AI" (BoardOvercard(Check, AllIn))

  [<Fact>]
  let ``parseOopSpecialRules ovso works`` () = testParseOopSpecialRules "ovso" (BoardOvercard(Donk 67m, StackOff))

  [<Fact>]
  let ``parseOopSpecialRules 61 works`` () = testParseOopSpecialRules "61" (BoardOvercard(Donk 60m, CallEQ 25))

  [<Fact>]
  let ``parseOopSpecialRules 4 works`` () = testParseOopSpecialRules "4" (BoardOvercard(Check, StackOff))

  [<Fact>]
  let ``parseOopSpecialRules 44 works`` () = testParseOopSpecialRules "44" (BoardOvercard(Donk 62.5m, CallEQ 20))

  [<Fact>]
  let ``parseOopSpecialRules A works`` () = testParseOopSpecialRules "A" (BoardAce(OopDonk.AllIn, AllIn))

  [<Fact>]
  let ``parseOopSpecialRules A/f works`` () = testParseOopSpecialRules "A/f" (BoardAce(Donk 67m, Fold))

  [<Fact>]
  let ``parseOopSpecialRules Aso works`` () = testParseOopSpecialRules "Aso" (BoardAce(Donk 67m, StackOff))

  [<Fact>]
  let ``parseOopSpecialRules Bp GS works`` () = testParseOopSpecialRules "Bp GS" (PairedBoard (Check, CallEQ 14))

  [<Fact>]
  let ``parseOopSpecialRules Bp FD works`` () = testParseOopSpecialRules "Bp FD" (PairedBoard (Check, CallEQ 22))

  [<Fact>]
  let ``parseOopSpecialRules 22 works`` () = testParseOopSpecialRules "22" (PairedBoard (Donk 50m, CallEQ 20))

  [<Fact>]
  let ``parseOopSpecialRules Tpp works`` () = testParseOopSpecialRules "Tpp" (PairedBoard (OopDonk.AllIn, AllIn))

  [<Fact>]
  let ``parseOopSpecialRules 5 works`` () = testParseOopSpecialRules "5" (CheckCheck (Donk 75m, Call))

  [<Fact>]
  let ``parseOopSpecialRules 7 works`` () = testParseOopSpecialRules "7" (CheckCheck (Donk 75m, StackOff))

  [<Fact>]
  let ``parseOopSpecialRules ov ch ch works`` () = testParseOopSpecialRules "ov ch ch" (CheckCheckAndBoardOvercard (Donk 75m, CallEQ 22))

  [<Fact>]
  let ``parseOopSpecialRules 60 works`` () = testParseOopSpecialRules "60" KHighOnPaired

  [<Fact>]
  let ``parseOopSpecialRules Bov#62.5%/30 works`` () = testParseOopSpecialRules "Bov#62.5%/30" (BoardOvercard(Donk 62.5m, CallEQ 30))

  [<Fact>]
  let ``parseOopSpecialRules Bov#55%/c works`` () = testParseOopSpecialRules "Bov#55%/c" (BoardOvercard(Donk 55m, OopOnCBet.Call))

  [<Fact>]
  let ``parseOopSpecialRules Bov#55%/f works`` () = testParseOopSpecialRules "Bov#55%/f" (BoardOvercard(Donk 55m, OopOnCBet.Fold))

  [<Fact>]
  let ``parseOopSpecialRules Bovso#50% works`` () = testParseOopSpecialRules "Bovso#50%" (BoardOvercard(Donk 50m, StackOff))

  [<Fact>]
  let ``parseOopSpecialRules Chrov/20 works`` () = testParseOopSpecialRules "Chrov/20" (BoardOvercard(OopDonk.Check, RaiseGayCallEQ 20))

  [<Fact>]
  let ``parseOopSpecialRules Chrovb/10 works`` () = testParseOopSpecialRules "Chrovb/10" (CheckRaiseOvercardBluff(Raise(2.75m, OopOnCBet.CallEQ 10)))

  [<Fact>]
  let ``parseOopSpecialRules Chrovso works`` () = testParseOopSpecialRules "Chrovso" (BoardOvercard(OopDonk.Check, StackOffGay))

  [<Fact>]
  let ``parseOopSpecialRules Xoxo#50%/18 works`` () = testParseOopSpecialRules "Xoxo#50%/18" (CheckCheck(Donk 50m, CallEQ 18))

  [<Fact>]
  let ``parseOopSpecialRules Xoxo#RBS/so works`` () = testParseOopSpecialRules "Xoxo#RBS/so" (CheckCheck(RiverBetSizing, StackOff))

  [<Fact>]
  let ``parseOopSpecialRules Xoxo#ch/25 works`` () = testParseOopSpecialRules "Xoxo#ch/25" (CheckCheck(OopDonk.Check, CallEQ 25))

  [<Fact>]
  let ``parseOopSpecialRules parses multiple rules`` () =
    let actual = parseOopSpecialRules "AI#15, A, 61"
    let expected = [CallEQPlusXvsAI 15; BoardAce (OopDonk.AllIn, AllIn); BoardOvercard(Donk 60m, CallEQ 25)]
    Assert.Equal<System.Collections.Generic.IEnumerable<OopSpecialCondition>>(expected, actual)

  [<Fact>]
  let ``importFlopList imports list of boards`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopPART2.xlsx"
    use xl = useExcel fileName
    let actual = importFlopList "bluffy hero ch-r flop vs limp" xl.Workbook
    Assert.Equal(107, Seq.length actual)
    Assert.Equal("235", System.String.Join("", Seq.head actual |> Seq.map (fun x -> faceToChar x)))

  [<Fact>]
  let ``importRiverBetSizes imports list of river bet sizes`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopPART2.xlsx"
    use xl = useExcel fileName
    
    let rbs = importRiverBetSizes xl.Workbook |> Tuple.fst3
    Assert.Equal(5, rbs.Length)
    Assert.Equal({ MinPotSize = 241; MaxPotSize = 320; MinAllInPercentage = 58; MaxAllInPercentage = 70; BetSize = 68; MinChipsLeft = 80 }, rbs.[2])

    let rbv = importRiverBetSizes xl.Workbook |> Tuple.snd3
    Assert.Equal(5, rbv.Length)
    Assert.Equal({ MinPotSize = 161; MaxPotSize = 240; BetSize = 65; ThinBetSize = 65 }, rbv.[1])

    let f1rrrtv = importRiverBetSizes xl.Workbook |> Tuple.thrd3
    Assert.Equal(16, f1rrrtv.Length)
    Assert.Equal({ MinPotSize = 321; MaxPotSize = 355; F1RRRatio = 2.3m; RTVRatio = 3m }, f1rrrtv.[10])

  let trickyFileName = System.IO.Directory.GetCurrentDirectory() + @"\tricky.xlsx"

  [<Fact>]
  let ``importFloatFlopOopOptions imports float options for a sample cell`` () =
    use xl = useExcel trickyFileName
    let s = { defaultFlop with Board = parseBoard "2s2cJd"; Hand = parseSuitedHand "Qh2s" }
    let actual = importFloatFlopOopOptions xl.Workbook s
    let expected = ({ defaultOopOptions with Then = Call }, Some(Float ValueFloat), "tricky -> float OOP -> G14") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importFloatTurnOopOptions imports float options for a sample cell`` () =
    use xl = useExcel trickyFileName
    let s = { defaultTurn with Board = parseBoard "2s2cJsQs"; Hand = parseSuitedHand "5s3s" }
    let texture = { defaultTexture with Monoboard = 3 }
    let history = [notMotivated PreFlop 20 Action.Call; floatBluff Flop 30]
    let actual = importFloatTurnOopOptions xl.Workbook (handValueWithDraws s.Hand s.Board) texture s history
    let expected = ({ defaultOopOptions with First = Donk 50m; Then = StackOff }, Some(Float BluffFloat), "tricky -> float OOP -> T41") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importFloatTurnOopOptions imports float options with continuation`` () =
    use xl = useExcel trickyFileName
    let s = { defaultTurn with Board = parseBoard "2s3c6s7h"; Hand = parseSuitedHand "Ts9d" }
    let history = [notMotivated PreFlop 20 Action.Call; floatBluff Flop 30]
    let actual = importFloatTurnOopOptions xl.Workbook (handValueWithDraws s.Hand s.Board) defaultTexture s history
    let expected = ({ defaultOopOptions with Then = RaiseConditional { Size = 2.2m; MinStackRemaining = 0; MinStackPotRatio = 0.4m; On3Bet = AllIn } }, Some(Float (WithContinuation "62.5%/f")), "tricky -> float OOP -> N54") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importFloatRiverOopOptions imports float options for a sample cell`` () =
    use xl = useExcel trickyFileName
    let s = { defaultRiver with Board = parseBoard "2s2cJsQdKc"; Hand = parseSuitedHand "5s3s" }
    let history = [notMotivated PreFlop 20 Action.Call; notMotivated Flop 30 Action.Call; floatBluffCheck Turn]
    let actual = importFloatRiverOopOptions xl.Workbook (handValue s.Hand s.Board) defaultTexture s history
    let expected = ({ defaultOopOptions with Special = [CheckCheck(RiverBetSizing, CallEQIfRaised(8, 5))]; Scenario = "r8/5" }, "tricky -> float OOP -> AX8") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importFloatRiverOopOptions return continuation from turn if defined`` () =
    use xl = useExcel trickyFileName
    let s = { defaultRiver with Board = parseBoard "2s2cJsQdKc"; Hand = parseSuitedHand "5s3s" }
    let history = [
      notMotivated PreFlop 20 Action.Call
      floatBluff Flop 30
      floatBluffCheck Turn
      floatContinuation Turn 50 (Action.RaiseToAmount 120) "75%/so"]
    let actual = importFloatRiverOopOptions xl.Workbook (handValue s.Hand s.Board) defaultTexture s history
    let expected = ({ defaultOopOptions with First = Donk 75m; Then = StackOff }, "tricky -> float OOP -> continuation") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importFloatFlopIpOptions imports float options for a sample cell`` () =
    use xl = useExcel trickyFileName
    let s = { defaultFlop with Board = parseBoard "2s2cJd"; Hand = parseSuitedHand "Qh2s" }
    let actual = importFloatFlopIpOptions xl.Workbook s
    let expected = ((OnDonk.Call, OnDonkRaise.Undefined), Some(Float ValueFloat), "tricky -> float IP -> H14") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importFloatTurnIpCheckOptions imports float options for a sample cell`` () =
    use xl = useExcel trickyFileName
    let s = { defaultTurn with Board = parseBoard "2sTc9s8h"; Hand = parseSuitedHand "7s2d" }
    let history = [notMotivated PreFlop 20 Action.Call; floatBluff Flop 30]
    let actual = importFloatTurnIpCheckOptions xl.Workbook (handValueWithDraws s.Hand s.Board) defaultTexture s history
    let expected = ({ defaultOopOptions with First = Donk 62.5m; Then = CallEQ 30 }, Some(Float BluffFloat), "tricky -> float IP -> N80") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importFloatTurnIpDonkOptions imports float options for a sample cell`` () =
    use xl = useExcel trickyFileName
    let s = { defaultTurn with Board = parseBoard "2sTc9s8h"; Hand = parseSuitedHand "7s2d" }
    let history = [notMotivated PreFlop 20 Action.Call; floatBluff Flop 30]
    let actual = importFloatTurnIpDonkOptions xl.Workbook (handValueWithDraws s.Hand s.Board) defaultTexture s history
    let expected = ((OnDonk.CallEQ 28, OnDonkRaise.Undefined), Some(Float BluffFloat), "tricky -> float IP -> AZ80") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importFloatRiverIpCheckOptions imports float options for a sample cell`` () =
    use xl = useExcel trickyFileName
    let s = { defaultRiver with Board = parseBoard "2s7cJs3d5d"; Hand = parseSuitedHand "5s3s" }
    let history = [notMotivated PreFlop 20 Action.Call; floatBluff Flop 30; floatBluffCheck Turn]
    let actual = importFloatRiverIpCheckOptions xl.Workbook (handValue s.Hand s.Board) defaultTexture s history
    let expected = ({ defaultOopOptions with First = RiverBetValue; Then = Call }, "tricky -> float IP -> CP35") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importFloatRiverIpDonkOptions imports float options for a sample cell`` () =
    use xl = useExcel trickyFileName
    let s = { defaultRiver with Board = parseBoard "2s7cJs3d5d"; Hand = parseSuitedHand "5s3s" }
    let history = [notMotivated PreFlop 20 Action.Call; floatBluff Flop 30; floatBluffCheck Turn]
    let actual = importFloatRiverIpDonkOptions xl.Workbook (handValue s.Hand s.Board) defaultTexture s history
    let expected = ((OnDonk.RaiseThinValue OnDonk.Call, OnDonkRaise.Call), "tricky -> float IP -> CZ35") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importIPTurnBooster imports turn booster options for a sample cell`` () =
    use xl = useExcel handStrengthFileName
    let s = { defaultTurn with Board = parseBoard "6s7c4d5d"; Hand = parseSuitedHand "5s3s" }
    let history = [notMotivated PreFlop 40 Action.Call]
    let actual = importIPTurnBooster xl.Workbook (handValueWithDraws s.Hand s.Board) defaultTexture s history
    let expected = ({ defaultOopOptions with First = Donk 40m; Then = CallEQ 8 }, "HandStrength -> postflop IP turn booster -> J40") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``historyPattern recognizes x/x + x/x + x`` () =
    let s = { defaultRiver with VillainBet = 0 }
    let history = [
      notMotivated PreFlop 40 Action.Call 
      notMotivated Flop 0 Action.Check
      notMotivated Turn 0 Action.Check]
    let actual = historyPattern s history
    Assert.Equal("x/x+x/x+x", actual)

  [<Fact>]
  let ``historyPattern recognizes db/c + x/c + db`` () =
    let s = { defaultRiver with VillainBet = 100 }
    let history = [
      notMotivated PreFlop 40 Action.Call 
      notMotivated Flop 50 (Action.RaiseToAmount 120)
      notMotivated Turn 0 (Action.RaiseToAmount 150)]
    let actual = historyPattern s history
    Assert.Equal("db/c+x/c+db", actual)

  [<Fact>]
  let ``historyPattern recognizes x/r + db + x`` () =
    let s = { defaultRiver with VillainBet = 0 }
    let history = [
      notMotivated PreFlop 40 Action.Call 
      notMotivated Flop 0 (Action.RaiseToAmount 50)
      notMotivated Flop 120 Action.Call
      notMotivated Turn 150 Action.Call]
    let actual = historyPattern s history
    Assert.Equal("x/r+db+x", actual)

  [<Fact>]
  let ``historyPattern recognizes hero 3bet on flop`` () =
    let history = [
      notMotivated PreFlop 40 Action.Call 
      notMotivated Flop 0 (Action.RaiseToAmount 50)
      notMotivated Flop 120 (Action.RaiseToAmount 220)
      notMotivated Turn 150 Action.Call]
    let actual = historyPattern defaultRiver history
    Assert.Equal("3bet", actual)

  [<Fact>]
  let ``historyPattern recognizes villain 3bet on turn`` () =
    let history = [
      notMotivated PreFlop 40 Action.Call 
      notMotivated Flop 0 Action.Check
      notMotivated Turn 50 (Action.RaiseToAmount 100)
      notMotivated Turn 200 Action.Call]
    let actual = historyPattern defaultRiver history
    Assert.Equal("3bet", actual)

  [<Fact>]
  let ``historyPattern recognizes 4bet on turn`` () =
    let history = [
      notMotivated PreFlop 40 Action.Call 
      notMotivated Flop 0 Action.Check
      notMotivated Turn 0 (Action.RaiseToAmount 50)
      notMotivated Turn 120 (Action.RaiseToAmount 220)
      notMotivated Turn 400 Action.Call]
    let actual = historyPattern defaultRiver history
    Assert.Equal("3bet", actual)

  [<Fact>]
  let ``importRiverPatterns imports correctly`` () =
    use xl = useExcel handStrengthFileName
    let actual = importRiverPatterns xl.Workbook
    Assert.Equal(26 * 3, actual.Count)
    Assert.Equal(4, actual.["river - villain check - db+db+x"])
    Assert.Equal(2, actual.["river - villain donkbet to 30% - db/c+db/c+db"])
    Assert.Equal(1, actual.["river - villain donkbet 31%+ - x/x+db/c+db"])

  [<Fact>]
  let ``importRiverIP imports correct IP river options a sample cell vs check`` () =
    use xl = useExcel handStrengthFileName
    let s = { defaultRiver with Board = parseBoard "Qs2cJd5h7h"; Hand = parseSuitedHand "QhJc"; VillainBet = 0 }
    let h = [
      notMotivated Flop 0 Action.Check
      notMotivated Turn 0 (Action.RaiseToAmount 50)
      notMotivated Turn 0 Action.Call]
    let patterns = importRiverPatterns xl.Workbook
    let actual = importRiverIP xl.Workbook patterns (handValue s.Hand s.Board) s h defaultTexture
    let expected = ({ defaultOopOptions with First = RiverBetThinValue; Then = CallEQ 15 }, "river - villain check -> AF48") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importRiverIP imports correct IP river options a sample cell vs small donkbet`` () =
    use xl = useExcel handStrengthFileName
    let s = { defaultRiver with Board = parseBoard "Qs2c5hAd7h"; Hand = parseSuitedHand "7hKc"; VillainBet = 50; Pot = 250 }
    let h = [
      notMotivated Flop 30 Action.Call
      notMotivated Turn 40 Action.Call]
    let patterns = importRiverPatterns xl.Workbook
    let actual = importRiverIP xl.Workbook patterns (handValue s.Hand s.Board) s h defaultTexture
    let expected = ({ defaultOopOptions with Then = CallEQ 20 }, "river - villain donkbet to 30% -> V32") |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``importRiverIP imports correct IP river options a sample cell vs big donkbet`` () =
    use xl = useExcel handStrengthFileName
    let s = { defaultRiver with Board = parseBoard "Qs2c5h7hAd"; Hand = parseSuitedHand "5hKc"; VillainBet = 50; Pot = 200 }
    let h = [
      notMotivated Flop 30 (Action.RaiseToAmount 60)
      notMotivated Flop 100 Action.Call
      notMotivated Turn 0 Action.Check]
    let patterns = importRiverPatterns xl.Workbook
    let actual = importRiverIP xl.Workbook patterns (handValue s.Hand s.Board) s h defaultTexture
    let expected = ({ defaultOopOptions with Then = CallEQ 10 }, "river - villain donkbet 31%+ -> V37") |> Some
    Assert.Equal(expected, actual)

  let testCbetMixup hand board h expected =
    use xl = useExcel handStrengthFileName
    let s = { defaultFlop with Hand = parseSuitedHand hand; Board = parseBoard board }
    let actual = importFlopCbetMixup xl.Workbook s h
    Assert.Equal(expected |> Some, actual)

  [<Fact>]
  let ``importFlopCbetMixup checks a sample hand after PFR`` () =
    let h = [ notMotivated PreFlop 20 (Action.RaiseToAmount 40) ]
    let expected = (defaultIpOptions, "HandStrength -> cbet mix up -> E13")
    testCbetMixup "9s2d" "2cKh2h" h expected

  [<Fact>]
  let ``importFlopCbetMixup checks a sample hand after limp`` () =
    let h = [ notMotivated PreFlop 20 Action.Call ]
    let expected = (defaultIpOptions, "HandStrength -> cbet mix up -> F13")
    testCbetMixup "9s2d" "2cKh2h" h expected

  [<Fact>]
  let ``importFlopCbetMixup checks a sample hand after call 3bet`` () =
    let h = [ notMotivated PreFlop 80 Action.Call ]
    let expected = (defaultIpOptions, "HandStrength -> cbet mix up -> G13")
    testCbetMixup "Qs7d" "2cKh2h" h expected

  [<Fact>]
  let ``importFlopCbetMixup bets a sample hand after call 3bet`` () =
    let h = [ notMotivated PreFlop 80 Action.Call ]
    let expected = ({ defaultIpOptions with CbetFactor = Always 50m }, "HandStrength -> cbet mix up -> G13")
    testCbetMixup "As3s" "2cKh2h"h expected

  [<Fact>]
  let ``importFlopCbetMixup checks a sample hand after limp-call`` () =
    let h = [ notMotivated PreFlop 70 Action.Call ]
    let expected = (defaultIpOptions, "HandStrength -> cbet mix up -> H13")
    testCbetMixup "Qs7d" "2cKh2h" h expected

  [<Fact>]
  let ``importFlopCbetMixup bets a sample hand after limp-call`` () =
    let h = [ notMotivated PreFlop 70 Action.Call ]
    let expected = ({ defaultIpOptions with CbetFactor = Always 60m }, "HandStrength -> cbet mix up -> H10")
    testCbetMixup "Jd8d" "2cTh2h" h expected

  [<Fact>]
  let ``importCbetMixupCheckRaise imports correct options for a sample cell`` () =
    use xl = useExcel handStrengthFileName
    let s = { defaultFlop with Hand = parseSuitedHand "KdTd"; Board = parseBoard "4cKh2h" }
    let actual = importCbetMixupCheckRaise xl.Workbook s (handValueWithDraws s.Hand s.Board)
    let expected = ({ defaultIpOptions with CheckRaise = OnCheckRaise.CallEQ 16 }, "HandStrength -> flop hand strength -> B12")
    Assert.Equal(expected, actual)