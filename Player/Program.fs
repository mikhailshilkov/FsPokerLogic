open Akka.FSharp
open System
open Interaction
open System.Drawing
open System.Collections.Generic
open Akka.Actor
open Recognition
open Recognition.ScreenRecognition
open Player
open Hands
open Preflop

type ClickTarget = (int * int * int * int)

type ClickAction =
  | ClickButton of ClickTarget
  | AmountToMax

type ClickerMessage = {
  WindowTitle: string
  Clicks: ClickAction[]
}

let executeClickAction window a =
  System.Threading.Thread.Sleep(500)
  match a with
  | AmountToMax -> printfn "Amount to max"
  | ClickButton (x, y, w, h) -> 
    let l = InteractionFacade.Focus(window)
    Clicker.clickRegion (l.X + x, l.Y + y, l.X + x + w, l.Y + y + h)

let clicker () =
  let imp (mailbox : Actor<'a>) (msg: ClickerMessage) =    
    msg.Clicks |> Array.iter (executeClickAction msg.WindowTitle)
  imp

let system = System.create "my-system" <| Configuration.defaultConfig()
let clickerRef = 
  spawn system "clicker-actor" (actorOf2 <| clicker())

type DecisionMessage = {
  WindowTitle: string
  Screen: Screen
}

let mapAction a buttons =
  let findButton names =
    buttons |> Array.tryFind (fun x -> Seq.exists (fun y -> x.Name = y) names)
  let button =
    match a with
    | Fold -> ["Check"; "Fold"]
    | Check -> ["Check"]
    | Call -> ["Call"; "AllIn"]
    | MinRaise -> ["RaiseTo"; "Bet"]
    | AllIn -> ["RaiseTo"; "Bet"]
    |> findButton

  match (a, button) with
  | (AllIn, Some b) -> [|AmountToMax; ClickButton(b.Region)|]
  | (_, Some b) -> [|ClickButton(b.Region)|]
  | (_, None) -> failwith "Could not find an appropriate button"

let decision (decide: Screen -> Preflop.Action option)  =
  let mutable lastScreen = None
  let imp (mailbox : Actor<'a>) msg =
    let screen = msg.Screen
    match lastScreen with
    | Some s when s = screen -> ()
    | _ ->
      lastScreen <- Some screen
      print screen |> Seq.iter (printfn "%s: %s" "Hand")
      let d = decide screen
      printfn "Decision is: %A" d
      let action = Option.map (fun x -> mapAction x screen.Actions) d
      match action with
      | Some a -> 
        printfn "Action is: %A" a
        clickerRef <! { WindowTitle = msg.WindowTitle; Clicks = a }
      | _ -> ()
  imp

let recognizer recognize =
  let mutable flowRef = null
  let imp (mailbox : Actor<'a>) (window : WindowInfo) =
    let result = recognize window.Bitmap
    if not(Array.isEmpty result.Actions) then
       if flowRef = null then flowRef <- spawn mailbox.Context "flow-actor" (actorOf2 <| decision Decide.decide')
       flowRef <! { WindowTitle = window.Title; Screen = result }
    //printfn "%s: %s" "Recognizer" window.Title
  imp

let recognize' b =
  b |> ScreenRecognition.recognizeScreen

let recognizeMock b =
  { TotalPot = Some 440; HeroStack = Some 460; VillainStack = Some 100; HeroBet = Some 140; VillainBet = Some 300; HeroHand = "8d8s"; Button = Villain; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Call"; Region = (5,6,7,8)};{Name="RaiseTo"; Region = (9,10,11,12)}|]; Blinds = Some { SB = 10; BB = 20 } }

let tableFinder (findWindows : unit -> WindowInfo seq) = 
  let children = new Dictionary<string, IActorRef>()
  let sendToRecognition context (window : WindowInfo) =
    let actorId = "recognizer-actor-" + window.TableName
    //printfn "Found table, sending to recognition"
    //printfn "%s" actorId
    let actorRef =
      if not(children.ContainsKey actorId) then
        let newAref = spawn context actorId (actorOf2 <| recognizer recognize')
        children.Add(actorId, newAref)
        newAref
      else 
        children.[actorId]
    actorRef <! window
  let imp (mailbox : Actor<'a>) msg =
    //printfn "Searching tables..."
    findWindows ()
      |> Seq.iter (sendToRecognition mailbox.Context)
  imp

let findWindows' () =
  let screenSize = new Size(650, 490)
  let targetSize = new Size(650, 490)
  InteractionFacade.GetWindowList(screenSize, targetSize, "Heads Up ")

let findWindowsMock () =
  [ new WindowInfo(TableName = "Fake", Title = "Fake Title") ] |> seq

[<EntryPoint>]
let main argv = 

  let rc = Seq.head Decide.rules
  
  let tableFinderRef = 
    spawn system "table-finder-actor" (actorOf2 <| tableFinder findWindows')
  
  let pingMessage = 0
  system.Scheduler.ScheduleTellRepeatedly(TimeSpan.Zero, TimeSpan.FromSeconds(0.5), tableFinderRef, pingMessage)
  printfn "%s" "Actor system started..."
  Console.ReadKey() |> ignore
  0
