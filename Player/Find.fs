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

  let findActor recognizer = 
    let children = new Dictionary<string, IActorRef>()
    let sendToRecognition context (window : WindowInfo) =
      let actorId = "recognizer-actor-" + window.TableName
      //printfn "Found table, sending to recognition"
      //printfn "%s" actorId
      let actorRef =
        if not(children.ContainsKey actorId) then
          printfn "New table found %s" window.TableName
          let newAref = spawn context actorId (recognizer)
          children.Add(actorId, newAref)
          newAref
        else 
          children.[actorId]
      actorRef <! window
    let imp (mailbox : Actor<'a>) msg =
      if msg > 0 then 
        printfn "Searching tables..."
        findWindows' ()
          |> Seq.iter (sendToRecognition mailbox.Context)
    imp
