namespace Player

open Recognition
open Recognition.ScreenRecognition
open Interaction
open Akka.FSharp
open Decide

module Recognize =

  let recognize' b = ScreenRecognition.recognizeScreen b

  let recognizeMock _ =
    { TotalPot = Some 440; HeroStack = Some 460; VillainStack = Some 100; HeroBet = Some 140; VillainBet = Some 300; HeroHand = "8d8s"; Button = Villain; Actions = [|{Name="Fold"; Region = (1,2,3,4)};{Name="Call"; Region = (5,6,7,8)};{Name="RaiseTo"; Region = (9,10,11,12)}|]; Blinds = Some { SB = 10; BB = 20 }; Board = "5h6s7c"; Sitout = Unknown }  

  let recognizeActor (window : WindowInfo) =
    let result = recognize' window.Bitmap
    let canAct = System.String.IsNullOrEmpty(result.Board) || result.Button = Hero
    if canAct && not(Array.isEmpty result.Actions) then
      Some { WindowTitle = window.Title; TableName = window.TableName; Screen = result; Bitmap = window.Bitmap }
    else None    