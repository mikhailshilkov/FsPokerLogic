open System
open Excel.Import
open Hands
open Cards.HandValues
open PostFlop.HandValue
open PostFlop.Decision
open PostFlop.Facade

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

  Console.Write "Opening excel files..."
  let fileNameFlopTurn = System.IO.Directory.GetCurrentDirectory() + @"\PostflopIP.xlsx"
  use xlFlopTurn = useExcel fileNameFlopTurn
  let fileNameTurnDonk = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
  use xlTurnDonkRiver = useExcel fileNameTurnDonk
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
  let suitedHand = parseSuitedHand handString

  printf "\nPot preflop is %A. Please enter board (e.g. 9s8c7d): " (bb*4)
  let boardString = Console.ReadLine()
  let board = parseBoard boardString
  let value = handValueWithDraws suitedHand board
  let special = boardTexture board
  printfn "Hand value is: %A" value

  let villainBet = enterNumber "Please enter the villain bet (0 for check)" 0 (villainStack - 40)
  let heroBet = if villainBet > 0 then enterNumber "Please enter the (previous) hero bet (can be zero)" 0 (heroStack - 40) else 0

  let s = { Hand = suitedHand; Board = board; Pot = bb * 4 + heroBet + villainBet; VillainStack = villainStack - bb*2 - villainBet; HeroStack = heroStack - bb*2 - heroBet; VillainBet = villainBet; HeroBet = heroBet; BB = bb }
  let decision = decidePostFlopNormal [] [] s value special xlFlopTurn.Workbook xlTurnDonkRiver.Workbook None

  try    
    match decision with
    | Some d -> printfn "Action is: %A. Press any key to quit." d
    | None -> printfn "No Action defined. Press any key to quit."
  with 
    | a -> 
      Console.WriteLine "Could not make decision"    
      Console.WriteLine a

  Console.ReadKey() |> ignore
  0 // return an integer exit code