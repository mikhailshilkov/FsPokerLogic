open Akka.FSharp
open System
open Akka.Actor
open Player
open Click
open Decide
open Recognize
open Find
open Enable
open ActorPatterns
open Excel.Import

[<EntryPoint>]
let main argv = 

  printfn "Loading rules..."
  let rc = Seq.head Decide.rulesLow
  let fileNameFlopTurn = System.IO.Directory.GetCurrentDirectory() + @"\PostflopIP.xlsx"
  use xlFlopTurn = useExcel fileNameFlopTurn
  let fileNameTurnDonk = System.IO.Directory.GetCurrentDirectory() + @"\HandStrength.xlsx"
  use xlTurnDonk = useExcel fileNameTurnDonk
  let fileNamePostFlopOop = System.IO.Directory.GetCurrentDirectory() + @"\PostflopOOP.xlsx"
  use xlPostFlopOop = useExcel fileNamePostFlopOop
  let fileNameTricky = System.IO.Directory.GetCurrentDirectory() + @"\tricky.xlsx"
  use xlTricky = useExcel fileNameTricky

  let system = Configuration.defaultConfig() |> System.create "my-system"
  let spawnChild childActor (debug: string) name (mailbox : Actor<'a>) = 
    if debug <> null then printfn "%s" debug
    spawn mailbox.Context name childActor
  
  let clickerRef = actorOfSink click' |> spawn system "clicker-actor"

  let decider = actorOfStatefulConvert (decisionActor xlFlopTurn.Workbook xlTurnDonk.Workbook xlPostFlopOop.Workbook xlTricky.Workbook) None clickerRef

  let recognizer = actorOfConvertToChild recognizeActor (spawnChild decider null "decider")

  let tableFinderRef = 
    actorOfConvertToChildren findActor (spawnChild recognizer "New table found")
    |> spawn system "table-finder-actor"

  let enablerRef = actorOfConvert enableActor tableFinderRef |> spawn system "enabler-actor"
  
  system.Scheduler.ScheduleTellRepeatedly(TimeSpan.Zero, TimeSpan.FromSeconds(0.5), enablerRef, 1)

  printfn "%s" "\nActor system started... Press any key to exit."
  Console.ReadKey() |> ignore
  0
