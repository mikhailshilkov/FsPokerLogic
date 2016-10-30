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
    { TotalPot = Some 825; HeroStack = Some 175; VillainStack = Some 0; HeroBet = Some 170; VillainBet = Some 385; HeroHand = "8s2s"; Board = "8d3h5d3dAs"; Blinds = Some { SB = 15; BB = 30 }; Button = Hero; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Check"; Region = (5,6,7,8)};{Name="Raise"; Region = (9,10,11,12)}|]; Sitout = Unknown; VillainName = "Ctid80" }

  let recognizeActor (window : WindowInfo) =
    let result = recognize' window.Bitmap
    let heroBet = defaultArg result.HeroBet 0
    let villainBet = defaultArg result.VillainBet 0
    let bb = defaultArg (result.Blinds |> Option.map (fun b -> b.BB)) 0
    let hasHand = String.IsNullOrEmpty result.HeroHand |> not
    if result.Sitout = Hero ||
      (hasHand && (Array.length result.Actions > 1) 
       && (heroBet < villainBet || (heroBet = villainBet && heroBet <= bb))) then
      Some { WindowTitle = window.Title; TableName = window.TableName; Screen = result; Bitmap = window.Bitmap }
    else None    