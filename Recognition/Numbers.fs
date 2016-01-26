namespace Recognition

open System
open System.Drawing

module Numbers =
  type BW = B | W

  let zero  = [[B;W;W;W;W;W;W;B];[W;B;B;B;B;B;B;W];[W;B;B;B;B;B;B;W];[W;B;B;B;B;B;B;W];[B;W;W;W;W;W;W;B]]
  let one   = [[B;W;B;B;B;B;B;W];[W;W;W;W;W;W;W;W];[B;B;B;B;B;B;B;W]]
  let two   = [[B;W;B;B;B;B;W;W];[W;B;B;B;B;W;B;W];[W;B;B;B;W;B;B;W];[W;B;B;W;B;B;B;W];[B;W;W;B;B;B;B;W]]
  let three = [[B;W;B;B;B;B;W;B];[W;B;B;B;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[B;W;W;B;W;W;W;B]]
  let four  = [[B;B;B;W;W;B;B;B];[B;B;W;B;W;B;B;B];[B;W;B;B;W;B;B;B];[W;W;W;W;W;W;W;W];[B;B;B;B;W;B;B;B]]
  let six   = [[B;B;W;W;W;W;W;B];[B;W;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[B;B;B;B;W;W;W;B]]
  let eight = [[B;W;W;B;W;W;W;B];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[B;W;W;B;W;W;W;B]]

  let compareToDigit (d : list<list<BW>>) result (mask: list<list<BW>>) = 
    let equal = 
      mask.Length = d.Length &&
      mask |> List.zip d |> List.forall (fun (x,y) -> List.zip x y |> List.forall (fun (e,i) -> e = i))
    if equal then Some(result) else None

  let (|Zero|_|) (mask: list<list<BW>>) = compareToDigit zero 0 mask
  let (|One|_|) (mask: list<list<BW>>) = compareToDigit one 1 mask
  let (|Two|_|) (mask: list<list<BW>>) = compareToDigit two 2 mask
  let (|Three|_|) (mask: list<list<BW>>) = compareToDigit three 3 mask
  let (|Four|_|) (mask: list<list<BW>>) = compareToDigit four 4 mask
  let (|Six|_|) (mask: list<list<BW>>) = compareToDigit six 6 mask
  let (|Eight|_|) (mask: list<list<BW>>) = compareToDigit eight 8 mask

  let matchNumber (mask: list<list<BW>>) =
    match mask with
    | Zero _ -> 0
    | One _ -> 1
    | Two _ -> 2
    | Three _ -> 3
    | Four _ -> 4
    | Six _ -> 6
    | Eight _ -> 8
    | _ -> -1

  let recognizeNumber (bitmap : Bitmap) =
    let isWhite (c : Color) =
      if c.B > 127uy && c.G > 127uy && c.R > 127uy then W
      else B

    let isSeparator (e : list<BW>) = List.forall ((=) B) e

    let folder (e : list<BW>) (state: list<list<list<BW>>>) = 
      match state with
      | cur::rest ->
          if isSeparator e then
            match cur with
            | _::_ -> []::state // add new list
            | _ -> state        // skip if we already have empty item
          else (e::cur)::rest   // add e to current list
      | _ -> [[e]]

    let whitePixels = 
      [0 .. bitmap.Width-1] 
      |> Seq.map (fun (x) -> [0 .. bitmap.Height-1] |> Seq.map (fun y -> bitmap.GetPixel(x, y)) |> Seq.map isWhite |> List.ofSeq)      
      |> List.ofSeq

    List.foldBack folder whitePixels []
    |> List.map matchNumber
    |> System.Collections.Generic.List
