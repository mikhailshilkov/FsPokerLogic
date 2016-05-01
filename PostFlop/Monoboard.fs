namespace PostFlop

open Hands
open Cards.HandValues
open Options

module Monoboard =
  let monoboard5River flushValue =
    match flushValue with
    | Board ->
      { Options.CbetFactor = CBet.Never
        CheckRaise = OnCheckRaise.Undefined
        Donk = OnDonk.CallEQ 12 }
    | Nut ->
      { Options.CbetFactor = CBet.Always 75m
        CheckRaise = OnCheckRaise.StackOff
        Donk = OnDonk.ForValueStackOffX 280 }
    | NotNut Jack | NotNut Queen | NotNut King ->
      { Options.CbetFactor = CBet.Always 50m
        CheckRaise = OnCheckRaise.CallEQ 18
        Donk = OnDonk.Call }
    | NotNut _ ->
      { Options.CbetFactor = CBet.Always 37.5m
        CheckRaise = OnCheckRaise.Fold
        Donk = OnDonk.CallEQ 25 }

  let monoboard4RiverWithFlush flushValue =
    match flushValue with
    | Nut ->
      { Options.CbetFactor = CBet.Always 62.5m
        CheckRaise = OnCheckRaise.StackOff
        Donk = OnDonk.ForValueStackOffX 250 }
    | NotNut Jack | NotNut Queen | NotNut King ->
      { Options.CbetFactor = CBet.Always 50m
        CheckRaise = OnCheckRaise.CallEQ 18
        Donk = OnDonk.Call }
    | _ ->
      { Options.CbetFactor = CBet.Never
        CheckRaise = OnCheckRaise.Fold
        Donk = OnDonk.CallEQ 22 }

  let monoboardRiver count madeValue =
    match count, madeValue with
    | _, FullHouse(_) | _, FourOfKind | _, StraightFlush -> None
    | 5, Flush(x) -> monoboard5River x |> Some
    | 4, Flush(x) -> monoboard4RiverWithFlush x |> Some
    | 4, _ -> 
      { Options.CbetFactor = CBet.Never
        CheckRaise = OnCheckRaise.Undefined
        Donk = OnDonk.Fold } |> Some
    | _ -> None

  let plusEQ d x = match d with | OnDonk.CallEQ y -> OnDonk.CallEQ (x + y) | y -> y

  let monoboard3Turn handValue o =
    match handValue.Made, handValue.FD with
    | Flush(_), _ ->
      { Options.CbetFactor = CBet.Always 75m
        CheckRaise = OnCheckRaise.StackOff
        Donk = OnDonk.ForValueStackOff }
    | Nothing, NoFD ->
      { Options.CbetFactor = CBet.Never
        CheckRaise = OnCheckRaise.Undefined
        Donk = OnDonk.CallEQ 10 }
    | x, Draw(d) when d = Ace || d = King || d = Queen || d = Jack ->
      match x with
      | Pair(Second(_)) | Pair Third | Pair Fourth | Pair Under | Nothing ->
        { Options.CbetFactor = CBet.Never
          CheckRaise = OnCheckRaise.Undefined
          Donk = OnDonk.Call }
      | _ -> o
    | Nothing, Draw(_) -> { o with Donk = plusEQ o.Donk 3 }
    | _ -> o

  let monoboard4Turn handValue =
    match handValue with
    | Flush(Nut) | Flush(NotNut King) | Flush(NotNut Queen) | Flush(NotNut Jack) ->
      { Options.CbetFactor = CBet.Always 50m
        CheckRaise = OnCheckRaise.Call
        Donk = OnDonk.ForValueStackOff }
    | Flush(_) ->
      { Options.CbetFactor = CBet.Always 37.5m
        CheckRaise = OnCheckRaise.Fold
        Donk = OnDonk.CallEQ 22 }
    | _ ->
      { Options.CbetFactor = CBet.Never
        CheckRaise = OnCheckRaise.Fold
        Donk = OnDonk.Fold }  

  let monoboardTurn count handValue o =
    match count with
    | 3 -> monoboard3Turn handValue o
    | 4 -> monoboard4Turn handValue.Made
    | _ -> o

  let monoboardFlop handValue o =
    match handValue.Made, handValue.FD with
    | Flush(_), _ -> 
      { Options.CbetFactor = CBet.Always 50m
        CheckRaise = OnCheckRaise.AllIn
        Donk = OnDonk.ForValueStackOff }
    | Pair(Top(_)), NoFD | TwoPair, NoFD | ThreeOfKind, NoFD | Straight(_), NoFD ->
      { o with Options.CbetFactor = CBet.Always 75m }
    | x, Draw(Ace) | x, Draw(King) | x, Draw(Queen) ->
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
