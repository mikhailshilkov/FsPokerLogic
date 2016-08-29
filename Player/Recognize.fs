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
    { TotalPot = Some 480; HeroStack = Some 330; VillainStack = Some 190; HeroBet = None; VillainBet = None; HeroHand = "7h5h"; Board = "Jd7c2c3s3d"; Button = Villain; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Check"; Region = (5,6,7,8)};{Name="Raise"; Region = (9,10,11,12)}|]; Blinds = Some { SB = 15; BB = 30 }; Sitout = Unknown; VillainName = "Ctid80" }

  let recognizeActor (window : WindowInfo) =
    let result = recognize' window.Bitmap
    let heroBet = defaultArg result.HeroBet 0
    let villainBet = defaultArg result.VillainBet 0
    let bb = defaultArg (result.Blinds |> Option.map (fun b -> b.BB)) 0
    if not(String.IsNullOrEmpty result.HeroHand)
       && not(Array.isEmpty result.Actions) 
       && (heroBet < villainBet || (heroBet = villainBet && heroBet <= bb)) then
      Some { WindowTitle = window.Title; TableName = window.TableName; Screen = result; Bitmap = window.Bitmap }
    else None    