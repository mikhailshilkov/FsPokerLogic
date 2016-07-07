namespace Player

open Recognition
open Recognition.ScreenRecognition
open Interaction
open Akka.FSharp
open Decide

module Recognize =

  let recognize' b = ScreenRecognition.recognizeScreen b

  let recognizeMock _ =
    { TotalPot = Some 45; HeroStack = Some 875; VillainStack = Some 80; HeroBet = Some 15; VillainBet = Some 30; HeroHand = "TcTd"; Button = Hero; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Call"; Region = (5,6,7,8)};{Name="RaiseTo"; Region = (9,10,11,12)}|]; Blinds = Some { SB = 15; BB = 30 }; Board = null; Sitout = Unknown; VillainName = "718135" }

  let recognizeActor (window : WindowInfo) =
    let result = recognizeMock window.Bitmap
    if not(Array.isEmpty result.Actions) 
       && (defaultArg result.HeroBet 0 <= defaultArg result.VillainBet 0) then
      Some { WindowTitle = window.Title; TableName = window.TableName; Screen = result; Bitmap = window.Bitmap }
    else None    