namespace Player

open System
open Recognition
open Recognition.ScreenRecognition
open Interaction
open Akka.FSharp
open Decide

module Recognize =

  let recognize' room bitmap = 
    match room with
    | "ipoker" -> ScreenRecognition.recognizeScreen bitmap
    | "winamax" -> WinamaxRecognition.recognizeScreenWinamax bitmap
    | _ -> failwith ("Unknown room " + room)

  let recognizeMock _ _ =
//    { TotalPot = Some 240; HeroStack = Some 380; VillainStack = Some 380; HeroBet = None; VillainBet = None; HeroHand = "6s5s"; Board = "8s7cTcJh"; 
//      Button = Villain; Blinds = Some { SB = 10; BB = 20 }; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Check"; Region = (5,6,7,8)};{Name="Raise"; Region = (9,10,11,12)}|]; Sitout = Unknown; VillainName = "noname" }

    { TotalPot = Some 60; HeroStack = Some 520; VillainStack = Some 420; HeroBet = Some 20; VillainBet = Some 40; HeroHand = "3h5h"; Board = null; 
      Button = Villain; Blinds = Some { SB = 10; BB = 20 }; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Check"; Region = (5,6,7,8)};{Name="Raise"; Region = (9,10,11,12)}|]; Sitout = Unknown; VillainName = "noname"; AmountInput = (13,14,15,16); Room = IPoker }

  let recognizeActor (window : WindowInfo) =
    let result = recognize' window.Room window.Bitmap
    let heroBet = defaultArg result.HeroBet 0
    let villainBet = defaultArg result.VillainBet 0
    let bb = defaultArg (result.Blinds |> Option.map (fun b -> b.BB)) 0
    let hasHand = String.IsNullOrEmpty result.HeroHand |> not
    let handActions = result.Actions |> Array.filter (fun a -> a.Name <> "Max" && a.Name <> "SitBack")
    if result.Sitout = Hero ||
      (hasHand && handActions.Length > 0
       && (heroBet < villainBet || (heroBet = villainBet && heroBet <= bb))) then
      Some { WindowTitle = window.Title; TableName = window.TableName; Screen = result; Bitmap = window.Bitmap }
    else None    