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
    Assert.Equal(OnDonk.ForValueStackOff, actual)
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
    Assert.Equal(OnDonk.ForValueStackOff, actual)
    closeExcel xl

  [<Fact>]
  let ``importRiver returns correct options for a sample cell`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
    let xl = openExcel fileName
    let actual = importRiver (fst xl) defaultTexture (FullHouse(Weak))
    let expected = { Options.CbetFactor = Always(37.5m); CheckRaise = OnCheckRaise.Fold; Donk = CallEQ 20 }
    Assert.Equal(expected, actual)
    closeExcel xl

  [<Fact>]
  let ``importRiver returns correct option when special conditions apply`` () =
    let fileName = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
    let xl = openExcel fileName
    let special = { defaultTexture with DoublePaired = true }
    let actual = importRiver (fst xl) special (Flush(NotNut Ten))
    let expected = { Options.CbetFactor = Always(37.5m); CheckRaise = OnCheckRaise.Fold; Donk = CallEQ 20 }
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