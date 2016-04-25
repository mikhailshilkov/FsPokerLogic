open System
open Excel.Import
open Hands
open Cards.HandValues
open PostFlop.Options
open PostFlop.Decision
open PostFlop.Import
open PostFlop.Texture
open PostFlop.HandValue

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
  let xlFlopTurn = openExcel fileNameFlopTurn
  let fileNameTurnDonk = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
  let xlTurnDonk = openExcel fileNameTurnDonk
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

  printf "\nPot preflop is %A. Please enter flop/turn (e.g. 9s8c7d): " (bb*4)
  let flopString = Console.ReadLine()
  let flop = parseBoard flopString
  let value = handValueWithDraws suitedHand flop
  printfn "Hand value is: %A" value

  let villainBet = enterNumber "Please enter the villain bet (0 for check)" 0 (villainStack - 40)
  let heroBet = if villainBet > 0 then enterNumber "Please enter the (previous) hero bet (can be zero)" 0 (heroStack - 40) else 0

  let street = if flop.Length = 4 then Turn else Flop
  let s = { Street = street; Pot = bb * 4 + heroBet + villainBet; VillainStack = villainStack - bb*2 - villainBet; HeroStack = heroStack - bb*2 - heroBet; VillainBet = villainBet; HeroBet = heroBet; BB = bb }
  let special = { StreetyBoard = isStreety 4 1 flop; DoublePairedBoard = isDoublePaired flop }
  let turnDonkOption = importTurnDonk (fst xlTurnDonk) special value
  let eo = importOptions (fst xlFlopTurn) hand flop 
  let o = 
    if street = Turn then 
      let turnFace = flop.[3].Face
      toTurnOptions turnFace (value.Made = Flush) (isFlushDrawWith2 suitedHand flop) turnDonkOption eo
    else
      toFlopOptions (isMonoboard flop) (isFlushDrawWith2 suitedHand flop) (canBeFlushDraw flop) eo
    |> augmentOptions suitedHand flop s

  try    
    match decide s o with
    | Some d -> printfn "Action is: %A. Press any key to quit." d
    | None -> printfn "No Action defined. Press any key to quit."
  with 
    | a -> 
      Console.WriteLine "Could not make decision"    
      Console.WriteLine a

  closeExcel xlFlopTurn
  closeExcel xlTurnDonk
  Console.ReadKey() |> ignore
  0 // return an integer exit code