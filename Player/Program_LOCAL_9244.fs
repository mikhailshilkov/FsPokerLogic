open Akka.FSharp
open System
open Akka.Actor
open Player
open Click
open Decide
open Recognize
open Find
open Interaction
open ActorPatterns

let enablerActor finderRef =
  let mutable i = 1
  let handler () =
    i <- (i + 1) % 2
    Console.WriteLine(if i = 0 then "Playing paused..." else "Playing resumed...")

  HotKeyManager.RegisterHotKey(System.Windows.Forms.Keys.Z, KeyModifiers.Control ||| KeyModifiers.Shift) |> ignore
  HotKeyManager.HotKeyPressed.AddHandler(fun _ _ -> handler())
    
  let imp _ _ = 
    finderRef <! i
  imp

[<EntryPoint>]
let main argv = 

  printfn "Loading rules..."
  //let rc = Seq.head Decide.rules

  let system = Configuration.defaultConfig() |> System.create "my-system"
  let spawnChild childActor name (mailbox : Actor<'a>) = 
    spawn mailbox.Context name childActor
  
  let clickerRef = actorOfSink click' |> spawn system "clicker-actor"

  let decider = actorOfStatefulConvert decisionConvert None clickerRef

  let recognizer = actorOfConvertToChild recognizeActor (spawnChild decider "decider")

  let tableFinder = actorOfConvertToChildren findActor (spawnChild recognizer)

  let tableFinderRef = tableFinder |> spawn system "table-finder-actor"

  let enablerRef = actorOf2 (enablerActor tableFinderRef) |> spawn system "enabler-actor"
  
  system.Scheduler.ScheduleTellRepeatedly(TimeSpan.Zero, TimeSpan.FromSeconds(0.5), enablerRef, 1)

  printfn "%s" "\nActor system started..."
  Console.ReadKey() |> ignore
  0
