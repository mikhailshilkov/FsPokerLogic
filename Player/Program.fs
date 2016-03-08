open Akka.FSharp
open System
open Akka.Actor
open Player
open Click
open Decide
open Recognize
open Find

[<EntryPoint>]
let main argv = 

  printfn "Loading rules..."
  //let rc = Seq.head Decide.rules

  let system = Configuration.defaultConfig() |> System.create "my-system"
  
  let clickerRef = 
    clickActor () 
    |> actorOf2 
    |> spawn system "clicker-actor"
  
  let tableFinderRef = 
    decideActor clickerRef 
    |> convertActor recognizeActor
    |> findActor
    |> actorOf2
    |> spawn system "table-finder-actor"
  
  system.Scheduler.ScheduleTellRepeatedly(TimeSpan.Zero, TimeSpan.FromSeconds(0.5), tableFinderRef, 1)

  printfn "%s" "\nActor system started..."
  Console.ReadKey() |> ignore
  0
