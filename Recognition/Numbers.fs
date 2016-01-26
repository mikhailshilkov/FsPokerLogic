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
  let five  = [[W;W;W;W;B;B;W;B];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;B;W;W;W;B]]
  let six   = [[B;B;W;W;W;W;W;B];[B;W;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[B;B;B;B;W;W;W;B]]
  let seven = [[W;B;B;B;B;B;B;B];[W;B;B;B;B;B;W;W];[W;B;B;B;W;W;B;B];[W;B;W;W;B;B;B;B];[W;W;B;B;B;B;B;B]]
  let eight = [[B;W;W;B;W;W;W;B];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[W;B;B;W;B;B;B;W];[B;W;W;B;W;W;W;B]]
  let nine  = [[B;W;W;W;B;B;B;B];[W;B;B;B;W;B;B;W];[W;B;B;B;W;B;B;W];[W;B;B;B;W;B;W;B];[B;W;W;W;W;W;B;B]]

  let comma = [[B;B;B;B;B;B;W;W]]

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
  let (|Five|_|) (mask: list<list<BW>>) = compareToDigit five 5 mask
  let (|Six|_|) (mask: list<list<BW>>) = compareToDigit six 6 mask
  let (|Seven|_|) (mask: list<list<BW>>) = compareToDigit seven 7 mask
  let (|Eight|_|) (mask: list<list<BW>>) = compareToDigit eight 8 mask
  let (|Nine|_|) (mask: list<list<BW>>) = compareToDigit nine 9 mask

  let (|Comma|_|) (mask: list<list<BW>>) = compareToDigit comma 9 mask

  let matchNumber (mask: list<list<BW>>) =
    match mask with
    | Zero _ -> '0'
    | One _ -> '1'
    | Two _ -> '2'
    | Three _ -> '3'
    | Four _ -> '4'
    | Five _ -> '5'
    | Six _ -> '6'
    | Seven _ -> '7'
    | Eight _ -> '8'
    | Nine _ -> '9'
    | Comma _ -> ','
    | _ -> '?'

  let recognizeNumber (bitmap : Bitmap) =
    let isWhite (c : Color) =
      if c.B > 127uy && c.G > 127uy && c.R > 127uy then W
      else B

    let isSeparator (e : list<BW>) = List.forall ((=) B) e

    let removeTrailingSeparators l = Seq.skipWhile (fun x -> Seq.forall ((=) B) x) l

    let folder (e : list<BW>) (state: list<list<list<BW>>>) = 
      match state with
      | cur::rest ->
          if isSeparator e then
            match cur with
            | _::_ -> []::state // add new list
            | _ -> state        // skip if we already have empty item
          else (e::cur)::rest   // add e to current list
      | _ -> [[e]]

    let pixels = 
      Array2D.init bitmap.Width bitmap.Height (fun x y -> isWhite (bitmap.GetPixel(x, y)))

    let maxWidth = Array2D.length1 pixels - 1
    let maxHeight = Array2D.length2 pixels - 1
    let first = [0..maxHeight] |> Seq.findIndex (fun x -> Array.exists ((=) W) pixels.[0..maxWidth, x])
    let last = [0..maxHeight] |> Seq.findIndexBack (fun x -> Array.exists ((=) W) pixels.[0..maxWidth, x])


    let whitePixels = 
      [0..maxWidth] 
      |> Seq.map (fun x -> [first..last] |> Seq.map (fun y -> pixels.[x, y]) |> List.ofSeq)
      |> removeTrailingSeparators
      |> Seq.rev
      |> removeTrailingSeparators
      |> Seq.rev

    Seq.foldBack folder whitePixels []
    |> List.map matchNumber
    |> Array.ofSeq
    |> String.Concat
