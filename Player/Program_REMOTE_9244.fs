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

[<EntryPoint>]
let main argv = 

  printfn "Loading rules..."
  let rc = Seq.head Decide.rules

  let system = Configuration.defaultConfig() |> System.create "my-system"
  let spawnChild childActor (debug: string) name (mailbox : Actor<'a>) = 
    if debug <> null then printfn "%s" debug
    spawn mailbox.Context name childActor
  
  let clickerRef = actorOfSink click' |> spawn system "clicker-actor"

  let decider = actorOfStatefulConvert decisionActor clickerRef

  let recognizer = actorOfConvertToChild recognizeActor (spawnChild decider null "decider")

  let tableFinderRef = 
    actorOfConvertToChildren findActor (spawnChild recognizer "New table found")
    |> spawn system "table-finder-actor"

  let enablerRef = actorOfConvert enableActor tableFinderRef |> spawn system "enabler-actor"
  
  system.Scheduler.ScheduleTellRepeatedly(TimeSpan.Zero, TimeSpan.FromSeconds(0.5), enablerRef, 1)

  printfn "%s" "\nActor system started..."
  Console.ReadKey() |> ignore
  0
