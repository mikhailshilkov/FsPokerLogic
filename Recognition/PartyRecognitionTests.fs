module PartyRecognitionTests

open Xunit
open Recognition.ScreenRecognition
open Recognition.PartyRecognition
open System.Drawing
open System.IO

let testWithTitle title actual folderName =
  let testFile (name : string) =
    let image = new Bitmap(name)

    let result = recognizeScreenParty image title
    let expected = name.Substring(name.LastIndexOf('\\') + 1).Replace(".bmp", "").Replace("_", "")
    if expected <> "null" then
      Assert.Equal(expected, actual result)
    else
      Assert.Null(actual result)

  Directory.GetFiles(@"..\..\TestCases\Party\" + folderName) |> Array.iter testFile

let test actual folderName = testWithTitle "Unknown" actual folderName

[<Fact>]
let ``recognize bet sizes from predefined file`` () =
  test (fun r -> System.String.Format("{0}-{1}", Option.toNullable r.HeroBet, Option.toNullable r.VillainBet)) "Bets"

[<Fact>]
let ``recognize pot size from predefined file`` () =
  test (fun r -> System.String.Format("{0}", Option.toNullable r.TotalPot)) "Pot"

[<Fact>]
let ``recognize stakes from predefined file`` () =
  test (fun r -> System.String.Format("{0}-{1}", Option.toNullable r.HeroStack, Option.toNullable r.VillainStack)) "Stakes"

[<Fact>]
let ``recognize board from predefined file`` () =
  test (fun r -> if r.Board = null then "" else r.Board) "Flop"

[<Fact>]
let ``recognize hand from predefined file`` () =
  test (fun r -> if r.HeroHand = null then "" else r.HeroHand) "Hand"

[<Fact>]
let ``recognize blinds from predefined file`` () =
  test (fun r -> sprintf "%A-%A" r.Blinds.Value.SB r.Blinds.Value.BB) "Blinds"

[<Fact>]
let ``recognize dealer (button) from predefined file`` () =
  test (fun r -> match r.Button with | Hero -> "H" | Villain -> "V" | _ -> "U") "Dealer"

[<Fact>]
let ``recognize actions from predefined file`` () =
  let formatActions a =
    let names = a |> Array.filter (fun a -> a.Name <> "Max") |> Array.map (fun x -> x.Name)
    System.String.Join("-", names)
  test (fun r -> r.Actions |> formatActions) "ActionsButtons"

//[<Fact>]
//let ``recognize sitout from predefined file`` () =
//  test (fun r -> 
//    match r.Sitout with 
//    | Hero -> "Hero" + r.HeroHand
//    | Villain -> "Villain" 
//    | _ -> "Nobody") "Sitout"

[<Fact>]
let ``recognize input bet size from predefined file`` () =
  let testFile (name : string) =
    let image = new Bitmap(name)

    let result = recognizeBetSizeParty image
    let expectedString = name.Substring(name.LastIndexOf('\\') + 1).Replace(".bmp", "").Replace("_", "")
    let expected = if expectedString = "" then None else expectedString |> int |> Some
    Assert.Equal(expected, result)

  Directory.GetFiles(@"..\..\TestCases\Party\BetSizes") |> Array.iter testFile