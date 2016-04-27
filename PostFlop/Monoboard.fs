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
        Donk = OnDonk.ForValueStackOff }
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
        Donk = OnDonk.ForValueStackOff }
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

  let monoboardTurn count handValue =
    match count, handValue.Made, handValue.FD with
    | 3, Flush(_), _ ->
      { Options.CbetFactor = CBet.Always 75m
        CheckRaise = OnCheckRaise.StackOff
        Donk = OnDonk.ForValueStackOff } |> Some
    | 3, Nothing, NoFD ->
      { Options.CbetFactor = CBet.Never
        CheckRaise = OnCheckRaise.Undefined
        Donk = OnDonk.CallEQ 10 } |> Some
    | _ -> None
