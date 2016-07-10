namespace Player

open Recognition
open Recognition.ScreenRecognition
open Interaction
open Akka.FSharp
open Decide

module Recognize =

  let recognize' b = ScreenRecognition.recognizeScreen b

  let recognizeMock _ =
    { TotalPot = Some 500; HeroStack = Some 500; VillainStack = Some 0; HeroBet = Some 100; VillainBet = Some 400; HeroHand = "TcTd"; Button = Villain; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Call"; Region = (5,6,7,8)};{Name="RaiseTo"; Region = (9,10,11,12)}|]; Blinds = Some { SB = 10; BB = 20 }; Board = null; Sitout = Unknown; VillainName = "77985" }

  let recognizeActor (window : WindowInfo) =
    let result = recognize' window.Bitmap
    if not(Array.isEmpty result.Actions) 
       && (defaultArg result.HeroBet 0 <= defaultArg result.VillainBet 0) then
      Some { WindowTitle = window.Title; TableName = window.TableName; Screen = result; Bitmap = window.Bitmap }
    else None    