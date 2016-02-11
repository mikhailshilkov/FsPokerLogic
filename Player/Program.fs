open Akka.FSharp
open System
open Interaction
open System.Drawing
open System.Collections.Generic
open Akka.Actor
open Recognition
open Recognition.ScreenRecognition
open Hands
open Preflop
open Import

let fileNameIP = System.IO.Directory.GetCurrentDirectory() + @"\IPinput.xlsx"
let rulesIP = importRuleFromExcel importRulesIP fileNameIP |> List.ofSeq
let decide stack = decideOnRules rulesIP stack

type DecisionMessage = {
  BigBlind: int
  Screen: Screen
}

let parseFullHand (s : string) =
  let card1 = parseFace s.[0]
  let card2 = parseFace s.[2]
  
  let sameSuit = 
    if card1 = card2 then false
    else s.[1] = s.[3]

  { Card1 = card1
    Card2 = card2
    SameSuit = sameSuit }


let decision (mailbox : Actor<'a>) (msg: DecisionMessage) =
  print msg.Screen |> Seq.iter (printfn "%s: %s" "Decision")
  let stack = min msg.Screen.HeroStack msg.Screen.VillainStack
  match stack with
  | Some s -> 
    let effectiveStack = decimal (s / msg.BigBlind)
    let action = decide effectiveStack [] (parseFullHand msg.Screen.HeroHand) // TODO history flow
    printfn "Action is: %A" action
  | _ -> ()

let recognizer () =
  let mutable flowRef = null
  let imp (mailbox : Actor<'a>) (window : WindowInfo) =
    //let result = ScreenRecognition.recognizeScreen <| window.Bitmap
    let result = { TotalPot = Some 180; HeroStack = Some 400; VillainStack = Some 300; HeroBet = None; VillainBet = None; HeroHand = "AhKd"; Button = Hero; Actions = "Fold|Call|RaiseTo"; Blinds = "10/20" }
    if not(String.IsNullOrEmpty result.Actions) then
       if flowRef = null then flowRef <- spawn mailbox.Context "flow-actor" (actorOf2 decision)
       flowRef <! { BigBlind = Int32.Parse(window.Title.Split('-').[1].Split('/').[1]); Screen = result }
    printfn "%s: %s" "Recognizer" window.Title
  imp

let tableFinder () = 
  let children = new Dictionary<string, IActorRef>()
  let sendToRecognition context (window : WindowInfo) =
    let tableName = window.Title.Split('-').[2].Trim()
    let actorId = "recognizer-actor-" + tableName
    printfn "%s" actorId
    let actorRef =
      if not(children.ContainsKey actorId) then
        let newAref = spawn context actorId (actorOf2 <| recognizer())
        children.Add(actorId, newAref)
        newAref
      else 
        children.[actorId]
    actorRef <! window
  let imp (mailbox : Actor<'a>) msg =
    let screenSize = new Size(650, 490)
    let targetSize = new Size(650, 490)
    InteractionFacade.GetWindowList(screenSize, targetSize, "Heads Up ")
      |> Seq.filter (fun w -> w.Title.Split('-').Length = 3)
      |> Seq.iter (sendToRecognition mailbox.Context)
    printfn "%s: %A" "Table finder" msg
  imp

[<EntryPoint>]
let main argv = 
  use system = System.create "my-system" <| Configuration.defaultConfig()
  
  let tableFinderRef = 
    spawn system "table-finder-actor" (actorOf2 <| tableFinder())
  
  let pingMessage = 0
  system.Scheduler.ScheduleTellRepeatedly(TimeSpan.Zero, TimeSpan.FromSeconds(0.5), tableFinderRef, pingMessage)
  printfn "%s" "Actor system started..."
  Console.ReadKey() |> ignore
  0
