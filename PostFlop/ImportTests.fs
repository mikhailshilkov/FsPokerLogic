namespace PostFlop

module ImportTests =

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
  let defaultOptions = { First = Check; Then = Fold; Special = []; Scenario = null; SpecialScenario = null }
  let defaultTurn = { Hand = parseSuitedHand "7s2c"; Board = parseBoard "KdJs6c2d"; Pot = 280; VillainStack = 340; HeroStack = 380; VillainBet = 100; HeroBet = 0; BB = 20 }
  let defaultRiver = { Hand = parseSuitedHand "7s2c"; Board = parseBoard "KdJs6c2d9d"; Pot = 280; VillainStack = 340; HeroStack = 380; VillainBet = 100; HeroBet = 0; BB = 20 }
  let defaultHistory = [
      { Action = Action.RaiseToAmount 40; Motivation = None; VsVillainBet = 20; Street = PreFlop }; 
      { Action = Action.RaiseToAmount 50; Motivation = None; VsVillainBet = 0; Street = Flop }]
  let defaultMade = { Made = Nothing; FD = NoFD; FD2 = NoFD; SD = NoSD }

  [<Theory>]
  [<InlineData("222", 6)>]
  [<InlineData("233", 19)>]
  [<InlineData("244", 31)>]
  [<InlineData("672", 53)>]
  [<InlineData("2AA", 96)>]
  [<InlineData("333", 97)>]
  [<InlineData("36T", 134)>]
  [<InlineData("444", 175)>]
  [<InlineData("AT6", 330)>]
  [<InlineData("KJQ", 446)>]
  [<InlineData("AAA", 460)>]
  let ``rowIndex for 222`` h expected =
    let hand = h |> Seq.map parseFace
    let actual = rowIndex hand
    Assert.Equal(expected, actual)

  [<Theory>]
  [<InlineData("3c4s5d", false, 14, 14, 15)>]
  [<InlineData("3c4s5dQd", false, 14, 14, 15)>]
  [<InlineData("3c4s5dQd", true, 8, 8, 8)>]
  let ``importOptions returns correct options for a sample cell`` boardString limpedPot ifPre1 ifPre2 ifPre3 =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopIP.xlsx"
    let xl = openExcel fileName
    let board = parseBoard boardString
    let hand = { Card1 = { Face = Ace; Suit = Clubs; }; Card2 = { Face = Two; Suit = Spades } }
    let actual = importOptions (fst xl) hand board limpedPot
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
    closeExcel xl

  [<Fact>]
  let ``importTurnDonk returns correct option for a sample cell`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
    let xl = openExcel fileName
    let actual = importTurnDonk (fst xl) { Made = Pair(Over); FD = NoFD; FD2 = NoFD; SD = NoSD } defaultTexture defaultTurn defaultHistory
    Assert.Equal(OnDonk.RaiseX 230, fst actual)
    Assert.Equal(OnDonkRaise.StackOff, snd actual)
    closeExcel xl

  [<Fact>]
  let ``importTurnDonk returns correct option when special conditions apply`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
    let xl = openExcel fileName
    let special = { defaultTexture with Streety = true }
    let actual = importTurnDonk (fst xl) { Made = Pair(Third); FD = NoFD; FD2 = NoFD; SD = NoSD } special defaultTurn defaultHistory
    Assert.Equal(OnDonk.CallEQ 10, fst actual)
    closeExcel xl

  [<Fact>]
  let ``importTurnDonk returns correct option on monobooard`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
    let xl = openExcel fileName
    let special = { defaultTexture with Monoboard = 4 }
    let actual = importTurnDonk (fst xl) { Made = Flush(NotNut Jack); FD = NoFD; FD2 = NoFD; SD = NoSD } special defaultTurn defaultHistory
    Assert.Equal(OnDonk.RaiseX 260, fst actual)
    Assert.Equal(OnDonkRaise.CallEQ 15, snd actual)
    closeExcel xl

  [<Fact>]
  let ``importRiver returns correct options for a sample cell`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
    let xl = openExcel fileName
    let actual = importRiver (fst xl) defaultTexture (FullHouse(Weak))
    let expected = { Options.CbetFactor = Always(37.5m); CheckRaise = OnCheckRaise.CallEQ 11; Donk = OnDonk.CallEQ 25; DonkRaise = OnDonkRaise.Undefined }
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importRiver returns correct option when special conditions apply`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
    let xl = openExcel fileName
    let special = { defaultTexture with DoublePaired = true }
    let actual = importRiver (fst xl) special (Flush(NotNut Ten))
    let expected = { Options.CbetFactor = Always(37.5m); CheckRaise = OnCheckRaise.CallEQ 11; Donk = OnDonk.CallEQ 20; DonkRaise = OnDonkRaise.Undefined }
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importRiver returns correct option when special conditions apply but no special action defined`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
    let xl = openExcel fileName
    let special = { defaultTexture with Streety = true; DoublePaired = true }
    let actual = importRiver (fst xl) special (Pair(Under))
    let expected = { Options.CbetFactor = Never; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Fold; DonkRaise = OnDonkRaise.Undefined }
    Assert.Equal(expected, actual)
    closeExcel xl

  let postflopOOPFileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopOOP.xlsx"

  [<Fact>]
  let ``importOopFlop returns correct options for a sample cell`` () =
    let xl = openExcel postflopOOPFileName
    let actual = importOopFlop (fst xl) "limp and check" { Made = Pair(Second Ten); FD = NoFD; FD2 = NoFD; SD = NoSD } defaultTexture
    let expected = { defaultOptions with Then = RaiseCallEQ 20 } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopFlop returns AI special option`` () =
    let xl = openExcel postflopOOPFileName
    let actual = importOopFlop (fst xl) "hero call raise pre" { Made = Pair(Second Ten); FD = NoFD; FD2 = NoFD; SD = NoSD } defaultTexture
    let expected = { defaultOptions with Then = CallEQ 34; Special = [CallEQPlusXvsAI 10] } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopTurn returns correct options for a sample cell`` () =
    let xl = openExcel postflopOOPFileName
    let texture = { defaultTexture with Monoboard = 4 }
    let actual = importOopTurn (fst xl) "limp and check" { Made = Flush(NotNut Queen); FD = NoFD; FD2 = NoFD; SD = NoSD } texture
    let expected = { defaultOptions with First = Donk(75m); Then = Call } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopTurn returns correct special options for a sample cell`` () =
    let xl = openExcel postflopOOPFileName
    let texture = { defaultTexture with Monoboard = 3; Streety = true }
    let actual = importOopTurn (fst xl) "limp and check" { Made = TwoPair; FD = Draw(NotNut(King)); FD2 = NoFD; SD = NoSD } texture
    let expected = { defaultOptions with Then = CallEQ 28 } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopTurn returns correct scenario for a sample cell`` () =
    let xl = openExcel postflopOOPFileName
    let actual = importOopTurn (fst xl) "hero call raise pre" { defaultMade with Made = Pair(Top(King)) } defaultTexture
    let expected = { defaultOptions with First = Donk(75m); Then = StackOff; Scenario = "r9" } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopTurn returns correct special scenario for a sample cell`` () =
    let xl = openExcel postflopOOPFileName
    let actual = importOopTurn (fst xl) "hero call raise pre" { defaultMade with Made = Pair(Second(King)) } defaultTexture
    let expected = { defaultOptions with Then = CallEQ 28; Special = [CheckCheck (Donk 75m, StackOff); BoardOvercard(Donk 67m, StackOff)]; SpecialScenario = "r9" } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

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
  let ``importOopRiver returns correct options for a sample cell`` () =
    let xl = openExcel postflopOOPFileName
    let actual = importOopRiver (fst xl) "limp and check" (FullHouse(Normal)) defaultTexture defaultRiver
    let expected = { defaultOptions with First = Donk(62.5m); Then = StackOff } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopRiver returns correct options for a sample cell with new hand value (two pair)`` () =
    let xl = openExcel postflopOOPFileName
    let s = { defaultRiver with Hand = parseSuitedHand "Qd4c"; Board = parseBoard "8s9d3hQh3c" }
    let actual = importOopRiver (fst xl) "hero call raise pre" TwoPair defaultTexture s
    let expected = { defaultOptions with Then = CallEQ 30 } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopRiver returns correct special options for a sample cell`` () =
    let xl = openExcel postflopOOPFileName
    let texture = { defaultTexture with DoublePaired = true }
    let actual = importOopRiver (fst xl) "limp and check" (Flush(NotNut Eight)) texture defaultRiver
    let expected = { defaultOptions with First = Donk(50m); Then = Fold } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopRiver returns correct scenario for a sample cell`` () =
    let xl = openExcel postflopOOPFileName
    let s = { defaultRiver with Board = parseBoard "8s9d3hQhTc" }
    let actual = importOopRiver (fst xl) "hero call raise pre" Nothing defaultTexture s
    let expected = { defaultOptions with Then = CallEQ 15; Scenario = "r8/5" } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopRiver returns correct scenario for 3-monoboard on river a sample cell`` () =
    let xl = openExcel postflopOOPFileName
    let s = { defaultRiver with Board = parseBoard "8s9dThQh3h" }
    let texture = { defaultTexture with Monoboard = 3 }
    let actual = importOopRiver (fst xl) "hero call raise pre" (Pair(Third)) texture s
    let expected = { defaultOptions with Then = CallEQ 23; Scenario = "r8/15" } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopRiver returns correct options on 4-monoboard`` () =
    let xl = openExcel postflopOOPFileName
    let texture = { defaultTexture with Monoboard = 4 }
    let actual = importOopRiver (fst xl) "limp and check" (FullHouse(Weak)) texture defaultRiver
    let expected = { defaultOptions with First = Donk(50m); Then = Fold } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopRiver returns correct special options on 4-monoboard`` () =
    let xl = openExcel postflopOOPFileName
    let texture = { defaultTexture with Monoboard = 4; Streety = true }
    let actual = importOopRiver (fst xl) "hero raise FV vs limp" ThreeOfKind texture defaultRiver
    let expected = { defaultOptions with First = Check; Then = CallEQ 14 } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopRiver returns correct options on 4-monoboard with flush`` () =
    let xl = openExcel postflopOOPFileName
    let texture = { defaultTexture with Monoboard = 4 }
    let actual = importOopRiver (fst xl) "limp and check" (Flush(NotNut King)) texture defaultRiver
    let expected = { defaultOptions with First = Donk(62.5m); Then = Call } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopRiver returns correct options on 5-monoboard with flush`` () =
    let xl = openExcel postflopOOPFileName
    let texture = { defaultTexture with Monoboard = 5 }
    let actual = importOopRiver (fst xl) "limp and check" (Flush(Board)) texture defaultRiver
    let expected = { defaultOptions with Then = CallEQ 25 } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  let testParseFlopOop s f t =
    let actual = parseOopOption s ""
    let expected = { defaultOptions with First = f; Then = t } |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``parseFlopOop ch/25 works`` () = testParseFlopOop "ch/25" Check (CallEQ 25)

  [<Fact>]
  let ``parseFlopOop 62.5%/30 works`` () = testParseFlopOop "62.5%/30" (Donk 62.5m) (CallEQ 30)

  [<Fact>]
  let ``parseFlopOop ch/r/f works`` () = testParseFlopOop "ch/r/f" Check (RaiseFold(2.75m))

  [<Fact>]
  let ``parseFlopOop ch/r/c works`` () = testParseFlopOop "ch/r/c" Check RaiseCall

  [<Fact>]
  let ``parseFlopOop 75%/c works`` () = testParseFlopOop "75%/c" (Donk 75m) Call

  [<Fact>]
  let ``parseFlopOop ch/r/20 works`` () = testParseFlopOop "ch/r/20" Check (RaiseCallEQ 20)

  [<Fact>]
  let ``parseFlopOop RBS/18 works`` () = testParseFlopOop "RBS/18" RiverBetSizing (CallEQ 18)

  [<Fact>]
  let ``parseFlopOop ch/25/ovso works`` () = 
    let actual = parseOopOption "ch/25@ovso" ""
    let expected = { defaultOptions with First = Check; Then = CallEQ 25; Special = [BoardOvercard(Donk 67m, StackOff)] } |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``parseFlopOop 62,5%/so*r9 and Ovso*r8 works`` () = 
    let actual = parseOopOption "62,5%/so*r9" "Ovso*r8"
    let expected = { First = Donk 62.5m; Then = StackOff; Scenario = "r9"; Special = [BoardOvercard(Donk 67m, StackOff)]; SpecialScenario = "r8" } |> Some
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
  let ``parseOopSpecialRules 61 works`` () = testParseOopSpecialRules "61" (BoardOvercard(Donk 100m, CallEQ 25))

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
  let ``parseOopSpecialRules Bovso#50% works`` () = testParseOopSpecialRules "Bovso#50%" (BoardOvercard(Donk 50m, StackOff))

  [<Fact>]
  let ``parseOopSpecialRules Chrov/20 works`` () = testParseOopSpecialRules "Chrov/20" (BoardOvercard(OopDonk.Check, RaiseGayCallEQ 20))

  [<Fact>]
  let ``parseOopSpecialRules Chrovb/10 works`` () = testParseOopSpecialRules "Chrovb/10" (CheckRaiseOvercardBluff(RaiseCallEQ 10))

  [<Fact>]
  let ``parseOopSpecialRules Chrovso works`` () = testParseOopSpecialRules "Chrovso" (BoardOvercard(OopDonk.Check, StackOffGay))

  [<Fact>]
  let ``parseOopSpecialRules Xoxo#50%/18 works`` () = testParseOopSpecialRules "Xoxo#50%/18" (CheckCheck(Donk 50m, CallEQ 18))

  [<Fact>]
  let ``parseOopSpecialRules parses multiple rules`` () =
    let actual = parseOopSpecialRules "AI#15, A, 61"
    let expected = [CallEQPlusXvsAI 15; BoardAce (OopDonk.AllIn, AllIn); BoardOvercard(Donk 100m, CallEQ 25)]
    Assert.Equal<System.Collections.Generic.IEnumerable<OopSpecialCondition>>(expected, actual)

  [<Fact>]
  let ``importFlopList imports list of boards`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopPART2.xlsx"
    let xl = openExcel fileName
    let actual = importFlopList "bluffy hero ch-r flop vs limp" (fst xl)
    Assert.Equal(107, Seq.length actual)
    Assert.Equal("235", System.String.Join("", Seq.head actual |> Seq.map (fun x -> faceToChar x)))
    closeExcel xl

  [<Fact>]
  let ``importRiverBetSizes imports list of donk river bet sizes`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopPART2.xlsx"
    let xl = openExcel fileName
    let actual = importRiverBetSizes (fst xl)
    Assert.Equal(5, actual.Length)
    Assert.Equal({ MinPotSize = 241; MaxPotSize = 320; MinAllInPercentage = 58; MaxAllInPercentage = 70; BetSize = 55; MinChipsLeft = 80 },
      actual.[2])
    closeExcel xl
