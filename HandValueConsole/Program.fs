open System
open Excel.Import
open Hands
open Cards.HandValues
open PostFlop.Options
open PostFlop.Decision
open PostFlop.Import
open PostFlop.Texture
open PostFlop.HandValue

[<EntryPoint>]
let main argv =   

  Console.Write "Opening excel files..."
  let fileNameTurnDonk = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
  let xlTurnDonk = openExcel fileNameTurnDonk
  Console.Write "\n"

  let mutable k = 'a'
  while k <> 'q' do
    Console.Write "\nPlease enter your hand (e.g. AsAc, 8d7h, Kh2h): "
    let handString = Console.ReadLine()
    let hand = parseFullHand handString
    let suitedHand = parseSuitedHand handString

    printf "\nPlease enter flop/turn (e.g. 9s8c7d): "
    let flopString = Console.ReadLine()
    let flop = parseBoard flopString
    let value = handValueWithDraws suitedHand flop
    printfn "Hand value is: %A" value

    let special = { StreetyBoard = isStreety 4 1 flop; DoublePairedBoard = isDoublePaired flop }
    printfn "Special conditions: %A" special

    let turnDonkOption = importTurnDonk (fst xlTurnDonk) special value
    printf "Turn donk action is: %A.\nPress any key to continue or 'q' to exit:" turnDonkOption
    k <- Console.ReadKey().KeyChar

  closeExcel xlTurnDonk
  0 // return an integer exit code