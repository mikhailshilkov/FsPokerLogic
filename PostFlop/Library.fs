namespace Microsoft.FSharp.Core

module Tuple =
  let fst3 (a, _, _) = a
  let snd3 (_, b, _) = b
  let thrd3 (_, _, c) = c

module Option =
  let addValue v o = o |> Option.map (fun o -> o, v)
  let add f o = o |> Option.map (fun o -> o, f o)
  let mapFst f o = o |> Option.map (fun (a, b) -> f a, b)
  let ofString s = if s = null || s = "" then None else Some s

