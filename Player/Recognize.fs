namespace Player

open System
open Recognition
open Recognition.ScreenRecognition
open Interaction
open Akka.FSharp
open Decide

module Recognize =

  let recognize' b = ScreenRecognition.recognizeScreen b

  let recognizeMock _ =
    { TotalPot = Some 200; HeroStack = Some 430; VillainStack = Some 370; HeroBet = None; VillainBet = Some 40; HeroHand = "7s7c"; Board = "8sQd7h7d"; Blinds = Some { SB = 10; BB = 20 }; Button = Hero; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Check"; Region = (5,6,7,8)};{Name="Raise"; Region = (9,10,11,12)}|]; Sitout = Unknown; VillainName = "Ctid80" }

  let recognizeActor (window : WindowInfo) =
    let result = recognize' window.Bitmap
    let heroBet = defaultArg result.HeroBet 0
    let villainBet = defaultArg result.VillainBet 0
    let bb = defaultArg (result.Blinds |> Option.map (fun b -> b.BB)) 0
    let hasHand = String.IsNullOrEmpty result.HeroHand |> not
    if (hasHand || result.Sitout = Hero)
       && (Array.length result.Actions > 1) 
       && (heroBet < villainBet || (heroBet = villainBet && heroBet <= bb)) then
      Some { WindowTitle = window.Title; TableName = window.TableName; Screen = result; Bitmap = window.Bitmap }
    else None    