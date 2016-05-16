namespace PostFlop

module ImportTests =

  open Import
  open Hands
  open Cards.HandValues
  open Options
  open Xunit
  open Microsoft.Office.Interop.Excel
  open System.Runtime.InteropServices
  open Excel.Import

  let defaultTexture = { Streety = false; DoublePaired = false; Monoboard = 2 }

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
  [<InlineData("3c4s5d")>]
  [<InlineData("3c4s5dQd")>]
  let ``importOptions returns correct options for a sample cell`` boardString =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopIP.xlsx"
    let xl = openExcel fileName
    let board = parseBoard boardString
    let hand = { Card1 = { Face = Ace; Suit = Clubs; }; Card2 = { Face = Two; Suit = Spades } }
    let actual = importOptions (fst xl) hand board
    let expected = { 
      CbetFactor = Always 50m
      CheckRaise = OnCheckRaise.CallEQ 1
      Donk = OnDonk.CallEQ 17
      DonkFlashDraw = Some OnDonk.ForValueStackOff
      TurnFVCbetCards = "8,Q"
      TurnFVCbetFactor = OrAllIn { Factor = 62.5m; IfStackFactorLessThan = 1.35m; IfPreStackLessThan = 14 }
      TurnCheckRaise = OnCheckRaise.StackOff
      TurnFBCbetCards = "T,J,K,A"
      TurnFBCbetFactor = OrCheck { Factor = 62.5m; IfStackFactorLessThan = 2.8m; IfPreStackLessThan = 18 }
      TurnFDCbetCards = "8,T,J,Q"
      TurnFDCbetFactor = OrAllIn { Factor = 62.5m; IfStackFactorLessThan = 2m; IfPreStackLessThan = 15 }
    }
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importTurnDonk returns correct option for a sample cell`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
    let xl = openExcel fileName
    let actual = importTurnDonk (fst xl) defaultTexture { Made = Pair(Over); FD = NoFD; SD = NoSD }
    Assert.Equal(OnDonk.ForValueStackOffX 250, actual)
    closeExcel xl

  [<Fact>]
  let ``importTurnDonk returns correct option when special conditions apply`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
    let xl = openExcel fileName
    let special = { defaultTexture with Streety = true }
    let actual = importTurnDonk (fst xl) special { Made = Pair(Third); FD = NoFD; SD = NoSD }
    Assert.Equal(OnDonk.Fold, actual)
    closeExcel xl

  [<Fact>]
  let ``importTurnDonk returns correct option when special conditions apply but no special action defined`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
    let xl = openExcel fileName
    let special = { defaultTexture with Streety = true; DoublePaired = true }
    let actual = importTurnDonk (fst xl) special { Made = Flush(NotNut Jack); FD = NoFD; SD = NoSD }
    Assert.Equal(OnDonk.ForValueStackOffX 250, actual)
    closeExcel xl

  [<Fact>]
  let ``importRiver returns correct options for a sample cell`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
    let xl = openExcel fileName
    let actual = importRiver (fst xl) defaultTexture (FullHouse(Weak))
    let expected = { Options.CbetFactor = Always(37.5m); CheckRaise = OnCheckRaise.CallEQ 11; Donk = OnDonk.CallEQ 20 }
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importRiver returns correct option when special conditions apply`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
    let xl = openExcel fileName
    let special = { defaultTexture with DoublePaired = true }
    let actual = importRiver (fst xl) special (Flush(NotNut Ten))
    let expected = { Options.CbetFactor = Always(37.5m); CheckRaise = OnCheckRaise.CallEQ 11; Donk = OnDonk.CallEQ 20 }
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importRiver returns correct option when special conditions apply but no special action defined`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
    let xl = openExcel fileName
    let special = { defaultTexture with Streety = true; DoublePaired = true }
    let actual = importRiver (fst xl) special (Pair(Under))
    let expected = { Options.CbetFactor = Never; CheckRaise = OnCheckRaise.Undefined; Donk = OnDonk.Fold }
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopFlop returns correct options for a sample cell`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopOOP.xlsx"
    let xl = openExcel fileName
    let actual = importOopFlop (fst xl) "limp and check" { Made = Pair(Second Ten); FD = NoFD; SD = NoSD } defaultTexture
    let expected = { First = Check; Then = CallEQ 27 } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopTurn returns correct options for a sample cell`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopOOP.xlsx"
    let xl = openExcel fileName
    let texture = { defaultTexture with Monoboard = 4 }
    let actual = importOopTurn (fst xl) "limp and check" { Made = Flush(NotNut Queen); FD = NoFD; SD = NoSD } texture
    let expected = { First = Donk(75m); Then = Call } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopRiver returns correct options for a sample cell`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopOOP.xlsx"
    let xl = openExcel fileName
    let actual = importOopRiver (fst xl) "limp and check" (FullHouse(Normal)) defaultTexture
    let expected = { First = Donk(62.5m); Then = StackOff } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopRiver returns correct options on 4-monoboard`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopOOP.xlsx"
    let xl = openExcel fileName
    let texture = { defaultTexture with Monoboard = 4 }
    let actual = importOopRiver (fst xl) "limp and check" (FullHouse(Weak)) texture
    let expected = { First = Donk(50m); Then = Fold } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopRiver returns correct options on 4-monoboard with flush`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopOOP.xlsx"
    let xl = openExcel fileName
    let texture = { defaultTexture with Monoboard = 4 }
    let actual = importOopRiver (fst xl) "limp and check" (Flush(NotNut King)) texture
    let expected = { First = Donk(50m); Then = Call } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importOopRiver returns correct options on 5-monoboard with flush`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopOOP.xlsx"
    let xl = openExcel fileName
    let texture = { defaultTexture with Monoboard = 5 }
    let actual = importOopRiver (fst xl) "limp and check" (Flush(Board)) texture
    let expected = { First = Check; Then = CallEQ 15 } |> Some
    Assert.Equal(expected, actual)
    closeExcel xl

  let testParseFlopOop s f t =
    let actual = parseOopOption s
    let expected = { First = f; Then = t } |> Some
    Assert.Equal(expected, actual)

  [<Fact>]
  let ``parseFlopOop ch/25 works`` () = testParseFlopOop "ch/25" Check (CallEQ 25)

  [<Fact>]
  let ``parseFlopOop 62.5%/30 works`` () = testParseFlopOop "62.5%/30" (Donk 62.5m) (CallEQ 30)

  [<Fact>]
  let ``parseFlopOop ch/r/f works`` () = testParseFlopOop "ch/r/f" Check RaiseFold

  [<Fact>]
  let ``parseFlopOop ch/r/c works`` () = testParseFlopOop "ch/r/c" Check RaiseCall

  [<Fact>]
  let ``parseFlopOop 75%/c works`` () = testParseFlopOop "75%/c" (Donk 75m) Call

  [<Fact>]
  let ``parseFlopOop ch/r/20 works`` () = testParseFlopOop "ch/r/20" Check (RaiseCallEQ 20)