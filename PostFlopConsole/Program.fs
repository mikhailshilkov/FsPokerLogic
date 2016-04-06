﻿open System
open Excel.Import
open Hands
open PostFlop.Options
open PostFlop.Decision
open PostFlop.Import
open PostFlop.Texture

let (|Int|_|) str =
   match System.Int32.TryParse(str) with
   | (true,int) -> Some(int)
   | _ -> None

let rec enterNumber text min max =
  printf "%s (%A-%A): " text min max  
  let input = Console.ReadLine()
  match input with
  | Int i when i >= min && i <= max -> i
  | _ ->
    Console.WriteLine "Not a valid number!"
    enterNumber text min max

[<EntryPoint>]
let main argv =   

  Console.Write "Reading excel file..."
  let fileName = System.IO.Directory.GetCurrentDirectory() + @"\PostflopIP.xlsx"
  let xl = openExcel fileName
  Console.Write "\n"

  //while true do
  let heroStack = enterNumber "Please enter the hero stack pre-flop" 1 999
  let villainStack = 1000 - heroStack

  let bb = enterNumber "Please enter the big blind" 20 500
  let sb = bb / 2
  let stack = min heroStack villainStack
  let effectiveStack = (decimal stack) / (decimal bb)

  printfn "Hero – %A chips, Villain – %A chips. Blind level %A/%A" heroStack villainStack sb bb

  Console.Write "\nPlease enter your hand (e.g. AsAc, 8d7h, Kh2h): "
  let handString = Console.ReadLine()
  let hand = parseFullHand handString
  let suitedHand = parseSuitedHand handString

  printf "\nPot preflop is %A. Please enter flop (e.g. 9s8c7d): " (bb*2)
  let flopString = Console.ReadLine()
  let flop = parseBoard flopString

  let villainBet = enterNumber "Please enter the villain bet (0 for check)" 0 (villainStack - 40)
  let heroBet = if villainBet > 0 then enterNumber "Please enter the (previous) hero bet (can be zero)" 0 (heroStack - 40) else 0

  let s = { Pot = bb * 4 + heroBet + villainBet; VillainStack = villainStack - bb*2; HeroStack = heroStack - bb*2; VillainBet = villainBet; HeroBet = heroBet; BB = bb }
  let o = importOptions (fst xl) hand flop |> toFlopOptions (isFlushDraw suitedHand flop) (canBeFlushDraw flop)

  try
    let d = decide s o
    printfn "Action is: %A. Press any key to quit." d
  with 
    | a -> 
      Console.WriteLine "Could not make decision"    
      Console.WriteLine a

  closeExcel xl
  Console.ReadKey() |> ignore
  0 // return an integer exit code