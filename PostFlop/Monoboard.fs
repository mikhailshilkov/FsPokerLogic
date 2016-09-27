namespace PostFlop

open Hands
open Cards.HandValues
open Options

module Monoboard =
  let plusEQ d x = match d with | OnDonk.CallEQ y -> OnDonk.CallEQ (x + y) | y -> y

  let monoboard3Turn handValue o =
    match handValue.Made, handValue.FD with
    | Flush(_), _ -> { o with Options.CbetFactor = CBet.Always 75m; CheckRaise = OnCheckRaise.StackOff }
    | Nothing, NoFD -> { o with Options.CbetFactor = CBet.Never; CheckRaise = OnCheckRaise.Undefined }
    | x, Draw(d) when d = Nut || d = NotNut(King) || d = NotNut(Queen) || d = NotNut(Jack) ->
      match x with
      | Pair(Second(_)) | Pair Third | Pair Fourth | Pair Under | Nothing ->
        { o with Options.CbetFactor = CBet.Never; CheckRaise = OnCheckRaise.Undefined }
      | _ -> o
    | _ -> o

  let monoboard4Turn handValue o =
    match handValue with
    | Flush(Nut) | Flush(NotNut King) | Flush(NotNut Queen) | Flush(NotNut Jack) ->
      { o with Options.CbetFactor = CBet.Always 50m; CheckRaise = OnCheckRaise.Call }
    | Flush(_) ->
      { o with Options.CbetFactor = CBet.Always 37.5m; CheckRaise = OnCheckRaise.Fold }
    | _ -> { o with Options.CbetFactor = CBet.Never; CheckRaise = OnCheckRaise.Fold }  

  let monoboardTurn count handValue o =
    match count with
    | 3 -> monoboard3Turn handValue o
    | 4 -> monoboard4Turn handValue.Made o
    | _ -> o

  let monoboardFlop handValue o =
    match handValue.Made, handValue.FD with
    | Flush(_), _ -> 
      { Options.CbetFactor = CBet.Always 50m
        CheckRaise = OnCheckRaise.AllIn
        Donk = OnDonk.ForValueStackOff
        DonkRaise = OnDonkRaise.StackOff }
    | Pair(Top(_)), NoFD | TwoPair, NoFD | ThreeOfKind, NoFD | Straight(_), NoFD ->
      { o with Options.CbetFactor = CBet.Always 75m }
    | x, Draw(d) when d = Nut || d = NotNut(King) || d = NotNut(Queen) ->
      match x with
      | Pair(Second _) | Pair(Third) | Pair(Under) | Nothing ->
        { o with CheckRaise = OnCheckRaise.StackOff; Donk = OnDonk.Call }
      | _ -> { o with CheckRaise = OnCheckRaise.StackOff }
    | x, Draw(_) ->
      match x with
      | Pair(Second _) | Pair(Third) | Pair(Under) | Nothing ->
        { o with Options.CbetFactor = CBet.Never; Donk = plusEQ o.Donk 6 }
      | _ -> { o with Donk = plusEQ o.Donk 6 }
    | _ -> o
