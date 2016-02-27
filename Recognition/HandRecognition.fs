namespace Recognition

open System.Drawing

module HandRecognition =
  type BW = B | W
  type CardPattern = {
    Card: string
    Pattern: BW array
  }

  let patterns = [|  
    { Card = "2"; Pattern = [|W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;W;W;W;W;W;W;B;B;B;W;W;W;W;B;B;B;B;W;W;W;W;W;B;B;B;B;W;W;W;B;B;B;B;W;W;W;W;B;B;B;B;B;B;W;W;W;B;B;W;W;W;W;W;W;B;B;W;W;B;B;W;W;W;B;B;W;W;W;W;W;B;B;W;W;W;B;B;W;W;W;B;B;B;W;W;B;B;B;B;W;W;W;B;B;W;W;W;W;B;B;B;B;B;B;B;W;W;W;W;B;B;W;W;W;W;W;B;B;B;B;W;W;W;W;W;W;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W|] }
    { Card = "3"; Pattern = [|W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;W;W;W;W;W;W;W;B;B;B;W;W;W;W;W;B;B;B;B;W;W;W;W;B;B;B;B;W;W;W;W;W;W;B;B;B;B;W;W;W;B;B;W;W;W;W;W;W;W;W;W;W;B;B;W;W;W;B;B;W;W;W;W;B;B;W;W;W;W;B;B;W;W;W;B;B;W;W;W;B;B;B;W;W;W;W;B;B;W;W;W;B;B;B;B;B;B;B;B;B;B;B;B;B;B;W;W;W;W;B;B;B;B;B;W;B;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W|] }
    { Card = "4"; Pattern = [|W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;B;B;B;W;B;B;B;W;W;W;W;W;W;W;W;W;B;B;B;W;W;W;B;B;W;W;W;W;W;W;W;B;B;B;W;W;W;W;B;B;B;W;W;W;W;W;W;B;B;B;B;B;B;B;B;B;B;B;B;B;B;W;W;W;B;B;B;B;B;B;B;B;B;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W|] }
    { Card = "5"; Pattern = [|W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;W;W;W;W;W;W;W;W;B;B;B;B;B;B;W;W;B;B;B;W;W;W;W;B;B;B;B;B;B;B;W;W;W;B;B;B;B;W;W;W;B;B;W;W;W;B;B;W;W;W;W;W;B;B;W;W;W;B;B;W;W;W;B;B;W;W;W;W;W;B;B;W;W;W;B;B;W;W;W;B;B;W;W;W;W;B;B;B;W;W;W;B;B;W;W;W;B;B;B;B;B;B;B;B;W;W;W;W;B;B;W;W;W;W;B;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W|] }
    { Card = "6"; Pattern = [|W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;W;W;W;W;W;W;W;W;W;W;W;B;B;B;B;B;B;B;B;B;W;W;W;W;W;W;B;B;B;W;B;B;B;B;B;B;B;B;B;W;W;W;B;B;W;W;W;B;B;W;W;W;W;W;B;B;W;W;W;B;W;W;W;W;B;W;W;W;W;W;W;B;B;W;W;W;B;B;W;W;W;B;B;W;W;W;W;W;B;B;W;W;W;B;B;B;W;W;B;B;B;B;B;B;B;B;B;W;W;W;W;W;B;W;W;W;B;B;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W|] }
    { Card = "7"; Pattern = [|W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;W;W;W;W;W;W;W;W;B;B;B;W;W;W;B;B;B;W;W;W;W;B;B;B;B;B;B;B;W;W;W;B;B;B;W;W;B;B;B;B;B;B;B;B;W;W;W;W;B;B;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W|] }
    //{ Card = "8"; Pattern = [|W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;W;W;W;W;B;B;B;B;W;W;W;W;W;W;B;B;B;B;B;W;B;B;B;B;B;B;W;W;W;W;B;B;W;W;W;B;B;B;W;W;W;W;B;B;W;W;W;B;W;W;W;W;W;B;W;W;W;W;W;B;B;W;W;W;B;W;W;W;W;W;B;W;W;W;W;W;B;B;W;W;W;B;B;W;W;W;B;B;B;W;W;W;B;B;B;W;W;W;W;B;B;B;B;B;W;B;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W|] }
    { Card = "8"; Pattern = [|W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;W;W;W;W;W;W;W;B;B;B;B;W;W;B;B;B;B;B;B;W;W;W;W;B;B;B;B;B;B;B;B;B;W;B;B;B;W;W;W;W;B;B;W;W;W;B;B;W;W;W;W;B;B;B;W;W;B;B;W;W;W;W;B;B;W;W;W;W;W;B;B;W;W;W;B;B;W;W;W;B;B;W;W;W;W;W;B;B;W;W;W;B;B;B;B;B;B;B;B;B;B;B;B;B;W;W;W;W;W;B;B;B;B;W;W;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W|] }
    { Card = "9"; Pattern = [|W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;B;W;W;W;W;B;W;W;W;W;W;W;B;B;B;B;B;B;B;B;W;B;B;B;W;W;W;W;B;B;W;W;W;W;W;B;B;W;W;W;B;B;W;W;W;B;B;W;W;W;W;W;B;B;W;W;W;B;B;W;W;W;B;B;W;W;W;W;W;B;B;W;W;W;B;B;W;W;W;B;B;B;W;W;W;B;B;W;W;B;B;B;B;W;W;W;W;B;B;B;B;B;B;B;B;B;B;B;B;W;W;W;W;W;W;W;W;B;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W|] }
    { Card = "T"; Pattern = [|W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;B;B;B;B;B;B;B;B;B;B;B;W;W;W;B;B;B;B;B;B;B;B;B;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;B;W;W;W;W;W;W;W;W;W;B;B;B;B;B;B;B;B;B;B;B;B;W;W;W;W;B;B;B;B;B;B;W;W;B;B;B;B;B;B;W;W;W;B;B;W;W;W;W;W;W;W;W;W;W;B;B;W;W;W;B;B;W;W;W;W;W;W;W;W;W;W;B;B;W;W;W;B;B;B;W;W;W;W;W;W;W;W;B;B;B;W|] }
    { Card = "J"; Pattern = [|W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;W;W;W;B;B;B;B;B;B;B;B;B;B;B;B;B;B;W;W;W;B;B;B;B;B;B;B;B;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W|] }
    { Card = "Q"; Pattern = [|W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;B;B;B;W;W;W;W;W;W;W;W;W;B;B;B;B;B;B;B;B;B;B;W;W;W;W;W;W;B;B;B;W;W;W;W;W;B;B;B;B;W;W;W;W;B;B;W;W;W;W;W;W;W;W;W;B;B;B;W;W;W;B;B;W;W;W;W;W;W;W;W;W;W;B;B;W;W;W;B;B;W;W;W;W;W;W;W;W;W;W;B;B;W;W;W;B;B;W;W;W;W;W;W;W;B;B;B;B;B;W;W;W;B;B;B;W;W;W;W;W;W;W;B;B;B;B;W;W;W;W;B;B;B;B;W;W;W;B;B;B;B;B;B;W;W;W;W;W;B;B;B;B;B;B;B;B;B;B;B;B;B;W;W;W;W;W;W;W;B;B;B;B;W;W;W;W;W;W|] }
    { Card = "K"; Pattern = [|W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;B;B;B;B;B;B;B;B;B;B;B;W;W;W;B;B;B;B;B;B;B;B;B;B;B;B;B;B;W;W;W;W;W;W;W;W;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;B;B;B;B;B;B;B;B;B;W;W;W;W;W;W;B;B;B;B;W;W;W;B;B;B;B;B;B;W;W;W;W;B;B;B;W;W;W;W;W;W;B;B;B;B;B;W;W;W;B;W;W;W;W;W;W;W;W;W;W;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W|] }
    { Card = "A"; Pattern = [|W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;W;W;W;W;W;W;W;W;W;W;B;B;B;B;B;B;B;W;W;W;W;W;W;W;B;B;B;B;B;B;B;B;W;W;W;W;W;W;B;B;B;B;B;B;B;B;B;B;W;W;W;W;W;W;B;B;B;B;B;W;W;W;B;B;B;W;W;W;W;W;W;B;B;B;B;W;W;W;W;B;B;B;W;W;W;W;W;W;B;B;B;B;B;B;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;B;B;B;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;B;B;B;B;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W;W|] }
  |]

  let findCardStart getPixel width height = 
    let isWhite (c : Color) = c.B > 127uy && c.G > 127uy && c.R > 127uy
    let firstX = [0..width] |> Seq.tryFindIndex (fun x -> [0..height] |> List.map (fun y -> getPixel x y |> isWhite)  |> Seq.exists id)
    let firstY = [0..height] |> Seq.tryFindIndex (fun y -> [0..width] |> List.map (fun x -> getPixel x y |> isWhite) |> Seq.exists id)
    (Option.map ((+) 1) firstX, Option.map ((+) 1) firstY)

  let getCardPattern getPixel width height =
    let isWhite (c : Color) =
      if c.B > 127uy && c.G > 127uy && c.R > 127uy then W
      else B

    seq { for x in 0 .. width - 1 do
            for y in 0 .. height - 1 do
              yield isWhite (getPixel x y)}    

  let getCardSuit getPixel width height =    
    let getSuit (c : Color) =
      match c with
      | _ when c.B < 127uy && c.G < 127uy && c.R > 127uy -> Some "h"
      | _ when c.B > 127uy && c.G < 127uy && c.R < 127uy -> Some "d"
      | _ when c.B < 127uy && c.G > 127uy && c.R < 127uy -> Some "c"
      | _ when c.B < 127uy && c.G < 127uy && c.R < 127uy -> Some "s"
      | _ -> None
    seq { for x in 0 .. width - 1 do
            for y in 0 .. height - 1 do
              yield getSuit (getPixel x y)}
    |> Seq.choose id
    |> Seq.countBy id
    |> Seq.maxBy (fun (v, c) -> c)
    |> fst

  let getCardValue patterns bws =
    let matchCount h p =
      Seq.zip h p
      |> Seq.map (fun (v1, v2) -> if v1 = v2 then 1 else -2)
      |> Seq.sum
    let rating = 
      patterns 
      |> Array.map (fun p -> (p, matchCount bws p.Pattern))
      |> Array.filter (fun (p, m) -> m > 0)
      |> Array.sortByDescending (fun (p_, m) -> m)
    rating
      |> Array.tryHead
      |> Option.map (fun (p, _) -> p)

  let getCardString getCardPattern getCardSuit =
    let value = getCardPattern |> getCardValue patterns
    let suit = getCardSuit
    match value with
    | Some v -> v.Card + suit
    | None -> null

  let recognizeCard getPixel width height = 
    let value = getCardPattern getPixel width height |> getCardValue patterns
    let suit = getCardSuit getPixel width height
    match value with
    | Some v -> v.Card + suit
    | None -> null

  let hasSpecialColor isColor getPixel width height =    
    seq { for x in 0 .. width - 1 do
            for y in 0 .. height - 1 do
              yield isColor (getPixel x y)}
    |> Seq.sumBy (fun x -> if x then 1 else 0)
    |> (*) 5
    |> (<) (width * height)

  let isButton x = 
    let isGreen (c : Color) = c.B < 127uy && c.G > 127uy && c.R < 127uy
    hasSpecialColor isGreen x

  let isFlop x = 
    let isWhite (c : Color) = c.B > 127uy && c.G > 127uy && c.R > 127uy
    hasSpecialColor isWhite x

  let parsePattern getPixel width height =
    getCardPattern getPixel width height
    |> Seq.map (fun x -> if x = B then "B" else "W") 
    |> String.concat ";"