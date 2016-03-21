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
    //let targetSize = new Size(433, 328)
    InteractionFacade.GetWindowList(screenSize, targetSize, "Heads Up ")

  let findWindowsMock () =
    [ new WindowInfo(TableName = "Fake", Title = "Fake Title") ] |> seq

  let findActor msg = 
    if msg > 0 then 
      //printfn "Finding tables..."
      findWindows' ()
        |> Seq.map (fun x -> ("recognizer-actor-" + x.TableName, x))
    else Seq.empty