namespace Player

open Recognition
open Recognition.ScreenRecognition
open Interaction
open Akka.FSharp
open Decide

module Recognize =

  let recognize' b = ScreenRecognition.recognizeScreen b

  let recognizeMock _ =
    { TotalPot = Some 440; HeroStack = Some 460; VillainStack = Some 100; HeroBet = Some 140; VillainBet = Some 300; HeroHand = "8d8s"; Button = Villain; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Call"; Region = (5,6,7,8)};{Name="RaiseTo"; Region = (9,10,11,12)}|]; Blinds = Some { SB = 10; BB = 20 }; HasFlop = false }

  let actor2 decider =
    let mutable decideRef = null
    let imp (mailbox : Actor<'a>) (window : WindowInfo) =
      let result = recognize' window.Bitmap
      if not result.HasFlop && not(Array.isEmpty result.Actions) then
         if decideRef = null then decideRef <- spawn mailbox.Context "decide-actor" (actorOf2 <| decider)
         decideRef <! { WindowTitle = window.Title; TableName = window.TableName; Screen = result; Bitmap = window.Bitmap }
      //printfn "%s: %s" "Recognizer" window.Title
    imp

  let actorOfWithChild f childActor mailbox =
    let spawnChild (mailbox : Actor<'a>) = 
      spawn mailbox.Context "decide-actor" childActor

    let rec imp state =
      actor {
        let newstate =
          match state with
          | Some s -> s
          | None -> spawnChild mailbox

        let! msg = mailbox.Receive()
        f newstate msg
        return! imp (Some newstate)
      }

    imp None

  let convertActorImp convert outputActor msg =
    convert msg |> Option.iter (fun x -> outputActor <! x) 

  let convertActor f =
    actorOfWithChild (convertActorImp f)

  let recognizeActor (window : WindowInfo) =
    let result = recognize' window.Bitmap
    if not result.HasFlop && not(Array.isEmpty result.Actions) then
      Some { WindowTitle = window.Title; TableName = window.TableName; Screen = result; Bitmap = window.Bitmap }
    else None
    