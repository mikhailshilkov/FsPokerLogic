namespace Player

open System
open Recognition
open Recognition.ScreenRecognition
open Interaction
open Akka.FSharp
open Decide

module Recognize =

  let recognize' room bitmap title = 
    match room with
    | "ipoker" -> ScreenRecognition.recognizeScreen bitmap
    | "winamax" -> WinamaxRecognition.recognizeScreenWinamax bitmap title
    | _ -> failwith ("Unknown room " + room)

  let recognizeMock _ _ _ =
//    { TotalPot = Some 540; HeroStack = Some 450; VillainStack = Some 10; HeroBet = None; VillainBet = Some 180; HeroHand = "5sQs"; Board = "7s8d5c5dKs"; 
//      Button = Hero; Blinds = Some { SB = 10; BB = 20 }; Actions = [|{Name="Max"; Region = (11,21,31,41)};{Name="Fold"; Region = (1,2,3,4)};{Name="Check"; Region = (5,6,7,8)};{Name="RaiseTo"; Region = (9,10,11,12)}|]; Sitout = Unknown; VillainName = "noname"; AmountInput = (13,14,15,16); Room = Winamax }
    { TotalPot = Some 440; HeroStack = Some 480; VillainStack = Some 80; HeroBet = None; VillainBet = Some 120; HeroHand = "9cTh"; Board = "Jh3dTd2cTc"; 
      Button = Hero; Blinds = Some { SB = 10; BB = 20 }; Actions = [|{Name="Max"; Region = (11,21,31,41)};{Name="Fold"; Region = (1,2,3,4)};{Name="Check"; Region = (5,6,7,8)};{Name="RaiseTo"; Region = (9,10,11,12)}|]; Sitout = Unknown; VillainName = "noname"; AmountInput = (13,14,15,16); Room = Winamax }

  let recognizeActor (window : WindowInfo) =
    let result = recognize' window.Room window.Bitmap window.Title
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