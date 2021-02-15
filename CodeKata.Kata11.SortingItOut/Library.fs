namespace CodeKata.Kata11.SortingItOut

open System

module private Sort =
    let rec insert collection item =
        match collection with
        | [] -> [item]
        | head :: tail when head >= item -> item :: head :: tail
        | head :: tail -> head :: (insert tail item)

[<RequireQualifiedAccess>]
module Rack =
    let create = []
    let rec add number rack = Sort.insert rack number

[<RequireQualifiedAccess>]
module HiddenMessage =
    let sortLetters (text: string) =
        text
        |> String.filter (Char.IsLetter)
        |> String.map (Char.ToLower)
        |> Seq.fold Sort.insert []
        |> String.Concat