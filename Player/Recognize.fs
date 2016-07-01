namespace Player

open Recognition
open Recognition.ScreenRecognition
open Interaction
open Akka.FSharp
open Decide

module Recognize =

  let recognize' b = ScreenRecognition.recognizeScreen b

  let recognizeMock _ =
    { TotalPot = Some 100; HeroStack = Some 250; VillainStack = Some 650; HeroBet = None; VillainBet = None; HeroHand = "7sKc"; Button = Villain; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Check"; Region = (5,6,7,8)};{Name="Bet"; Region = (9,10,11,12)}|]; Blinds = Some { SB = 10; BB = 20 }; Board = "7h6cQh4h4d"; Sitout = Unknown; VillainName = "mcpoker43" }

  let recognizeActor (window : WindowInfo) =
    let result = recognize' window.Bitmap
    if not(Array.isEmpty result.Actions) 
       && (defaultArg result.HeroBet 0 <= defaultArg result.VillainBet 0) then
      Some { WindowTitle = window.Title; TableName = window.TableName; Screen = result; Bitmap = window.Bitmap }
    else None    