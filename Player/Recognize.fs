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
    { TotalPot = Some 610; HeroStack = Some 390; VillainStack = Some 0; HeroBet = None; VillainBet = Some 370; HeroHand = "Jd8s"; Board = "7s8hTc4s"; Blinds = Some { SB = 10; BB = 20 }; Button = Hero; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Check"; Region = (5,6,7,8)};{Name="Raise"; Region = (9,10,11,12)}|]; Sitout = Unknown; VillainName = "Ctid80" }

  let recognizeActor (window : WindowInfo) =
    let result = recognizeMock window.Bitmap
    let heroBet = defaultArg result.HeroBet 0
    let villainBet = defaultArg result.VillainBet 0
    let bb = defaultArg (result.Blinds |> Option.map (fun b -> b.BB)) 0
    let hasHand = String.IsNullOrEmpty result.HeroHand |> not
    if (hasHand || result.Sitout = Hero)
       && (Array.length result.Actions > 1) 
       && (heroBet < villainBet || (heroBet = villainBet && heroBet <= bb)) then
      Some { WindowTitle = window.Title; TableName = window.TableName; Screen = result; Bitmap = window.Bitmap }
    else None    