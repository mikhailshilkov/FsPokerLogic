namespace Player

open Recognition
open Recognition.ScreenRecognition
open Interaction
open Akka.FSharp
open Decide

module Recognize =

  let recognize' b = ScreenRecognition.recognizeScreen b

  let recognizeMock _ =
    { TotalPot = Some 870; HeroStack = Some 130; VillainStack = Some 0; HeroBet = Some 240; VillainBet = Some 430; HeroHand = "Tc6dc"; Button = Villain; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Call"; Region = (5,6,7,8)};{Name="RaiseTo"; Region = (9,10,11,12)}|]; Blinds = Some { SB = 10; BB = 20 }; Board = null; Sitout = Unknown; VillainName = "starbucks5000" }

  let recognizeActor (window : WindowInfo) =
    let result = recognize' window.Bitmap
    if not(Array.isEmpty result.Actions) then
      Some { WindowTitle = window.Title; TableName = window.TableName; Screen = result; Bitmap = window.Bitmap }
    else None    