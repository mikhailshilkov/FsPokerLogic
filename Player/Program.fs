namespace Player

open Akka.FSharp
open System
open Akka.Actor
open Player
open Click
open Stats
open Decide
open Recognize
open Find
open Enable
open ActorPatterns
open Excel
open Excel.Import
open PostFlop.Import

module PlayerModule =

  let run() =
    let excel = new MemoryWorkstore(Serialization.loadRules())

    let rc = Seq.head Decide.rulesLow
    let xlHandStrength = excel.GetWorkbook "HandStrength.xlsx"
    let riverHistoryPatterns = importRiverPatterns xlHandStrength
    let xlPostFlopOop = excel.GetWorkbook "PostflopOOP.xlsx"
    let xlTricky = excel.GetWorkbook "tricky.xlsx"
    let xlBeavers = excel.GetWorkbook "mfck beavers.xlsx"
    let regs = Import.importRegs xlBeavers
    let xlFlopTurn = excel.GetWorkbook "PostflopIP.xlsx"

    let system = Configuration.defaultConfig() |> System.create "my-system"
    let spawnChild childActor (debug: string) name (mailbox : Actor<'a>) = 
      if debug <> null then printfn "%s" debug
      spawn mailbox.Context name childActor
  
    let clickerRef = actorOfSink click' |> spawn system "clicker-actor"
    let statsRef = actorOfSink stats' |> spawn system "stats-actor"

    let decider = actorOfStatefulConvert (decisionActor xlFlopTurn xlHandStrength xlPostFlopOop xlTricky xlBeavers regs riverHistoryPatterns) None (clickerRef, statsRef)

    let recognizer = actorOfConvertToChild recognizeActor (spawnChild decider null "decider")

    let tableFinderRef = 
      actorOfConvertToChildren findActor (spawnChild recognizer "New table found")
      |> spawn system "table-finder-actor"

    let enablerRef = actorOfConvert enableActor tableFinderRef |> spawn system "enabler-actor"
  
    system.Scheduler.ScheduleTellRepeatedly(TimeSpan.Zero, TimeSpan.FromSeconds(0.5), enablerRef, 1)

  [<EntryPoint>]
  let main argv = 

    printfn "Loading rules..."
    run()
    printfn "%s" "\nActor system started... Press any key to exit."
    Console.ReadKey() |> ignore
    0
