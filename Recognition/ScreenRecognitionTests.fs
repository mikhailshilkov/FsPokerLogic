module ScreenRecognitionTests

open Xunit
open Recognition.ScreenRecognition
open System.Drawing
open System.IO

let test actual folderName =
  let testFile (name : string) =
    let image = new Bitmap(name)

    let result = recognizeScreen image
    let expected = name.Substring(name.LastIndexOf('\\') + 1).Replace(".bmp", "")
    if expected <> "null" then
      Assert.Equal(expected, actual result)
    else
      Assert.Null(actual result)

  Directory.GetFiles(@"..\..\TestCases\" + folderName) |> Array.iter testFile

[<Fact>]
let ``recognize hand from predefined file`` () =
 test (fun r -> r.HeroHand) "Hand"

[<Fact>]
let ``recognize total pot and stack sizes from predefined file`` () =
  test (fun r -> System.String.Format("{0}-{1}-{2}", Option.toNullable r.TotalPot, Option.toNullable r.HeroStack, Option.toNullable r.VillainStack)) "PotStacks"

[<Fact>]
let ``recognize bet sizes from predefined file`` () =
  test (fun r -> System.String.Format("{0}-{1}", Option.toNullable r.HeroBet, Option.toNullable r.VillainBet)) "Bets"

[<Fact>]
let ``recognize position and actions from predefined file`` () =
  test (fun r -> System.String.Format("{0}-{1}", r.Actions.Replace("|", "-"), match r.Button with | Hero -> "H" | Villain -> "V" | Unknown -> "?")) "ActionsButtons"
