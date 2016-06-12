module ScreenRecognitionTests

open Xunit
open Recognition.ScreenRecognition
open System.Drawing
open System.IO

let test actual folderName =
  let testFile (name : string) =
    let image = new Bitmap(name)

    let result = recognizeScreen image
    let expected = name.Substring(name.LastIndexOf('\\') + 1).Replace(".bmp", "").Replace("_", "")
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
  let formatActions a =
    let names = a |> Array.map (fun x -> x.Name)
    System.String.Join("-", names)
  test (fun r -> System.String.Format("{0}-{1}", r.Actions |> formatActions, match r.Button with | Hero -> "H" | Villain -> "V" | Unknown -> "?")) "ActionsButtons"

[<Fact>]
let ``recognize blinds from predefined file`` () =
  test (fun r ->  sprintf "%A-%A" r.Blinds.Value.SB r.Blinds.Value.BB) "Blinds"

[<Fact>]
let ``recognize sitout from predefined file`` () =
  test (fun r -> 
    match r.Sitout with 
    | Hero -> "Hero" + r.HeroHand
    | Villain -> "Villain" 
    | _ -> "Nobody") "Sitout"

[<Fact>]
let ``recognize board from predefined file`` () =
  test (fun r -> r.Board) "Flop"

[<Fact>]
let ``recognize villain name from predefined file`` () =
  test (fun r -> r.VillainName.Replace("?", "-")) "Name"

[<Fact>]
let ``recognize input bet size from predefined file`` () =
  let testFile (name : string) =
    let image = new Bitmap(name)

    let result = recognizeBetSize image
    let expected = name.Substring(name.LastIndexOf('\\') + 1).Replace(".bmp", "").Replace("_", "")
    Assert.Equal(expected, result)

  Directory.GetFiles(@"..\..\TestCases\BetSizes") |> Array.iter testFile