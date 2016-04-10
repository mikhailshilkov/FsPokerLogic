namespace PostFlop

module ImportTests =

  open Import
  open Hands
  open Options
  open Xunit
  open Microsoft.Office.Interop.Excel
  open System.Runtime.InteropServices
  open Excel.Import

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
    let actual = importOptions (fst xl) { Face1 = Ace; Face2 = Two; SameSuit = false } board
    let expected = { CbetFactor = ForValue 50m; CheckRaise = OnCheckRaise.CallEQ 1; Donk = OnDonk.CallEQ 17; DonkFlashDraw = Some OnDonk.ForValueStackOff; TurnFVCbetCards = "8,Q"; TurnFVCbetFactor = ForValue 62.5m; TurnCheckRaise = OnCheckRaise.StackOff; TurnFBCbetCards = "T,J,K,A"; TurnFBCbetFactor = ForBluff 62.5m; TurnFDCbetCards = "8,T,J,Q"; TurnFDCbetFactor = ForValue 62.5m }
    Assert.Equal(expected, actual)
    closeExcel xl
