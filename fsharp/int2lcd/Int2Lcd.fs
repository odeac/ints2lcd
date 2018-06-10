module Int2Lcd

open System

type Lcd = string list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lcd =
    let empty = ["";"";""]
    let concat left right = 
        right
        |> Seq.zip left
        |> Seq.map (fun (l,r) -> l + r)
        |> List.ofSeq
    let toString = String.concat "\n"

let digits : Lcd list = 
    [ [" _ ";
       "| |";
       "|_|"];
      ["   ";
       "  |";
       "  |"];
      [" _ ";
       " _|";
       "|_ "];
      [" _ ";
       " _|";
       " _|"];
      ["   ";
       "|_|";
       "  |"];
      [" _ ";
       "|_ "
       " _|"];
      [" _ ";
       "|_ ";
       "|_|"];
      [" _ ";
       "  |";
       "  |"];
      [" _ ";
       "|_|";
       "|_|"];
      [" _ ";
       "|_|";
       " _|"]]



let private digit2Lcd (n:int) : Lcd = digits |> Seq.item n

let renderLcd (xs : int seq) : string =
    xs
    |> Seq.map digit2Lcd
    |> Seq.fold Lcd.concat Lcd.empty 
    |> Lcd.toString
