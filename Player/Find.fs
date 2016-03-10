namespace Player
 
module Find =
  open Interaction
  open System.Collections.Generic
  open Akka.FSharp
  open System.Drawing
  open Akka.Actor

  let findWindows' () =
    let screenSize = new Size(650, 490)
    let targetSize = new Size(650, 490)
    InteractionFacade.GetWindowList(screenSize, targetSize, "Heads Up ")

  let findWindowsMock () =
    [ new WindowInfo(TableName = "Fake", Title = "Fake Title") ] |> seq

  let findActor msg = 
    if msg > 0 then 
      printfn "Searching tables..."
      findWindowsMock ()
        |> Seq.map (fun x -> ("recognizer-actor-" + x.Title, x))
    else Seq.empty