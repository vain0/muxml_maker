namespace MuxmlMaker.Console

open System
open System.IO
open Basis.Core
open MuxmlMaker
open MuxmlMaker.Parser

module Program =

  let saveFileInStorage directoryName ext songName contents =
    option {
      let! root   = storagePath
      let dir     = Path.Combine(root, directoryName)
      let path    = Path.Combine(dir, songName + ext)
      if dir |> Directory.Exists then
        File.WriteAllText(path, contents)
    } |> ignore

  let dispatch filePath =
    let contents =
        File.ReadAllText(filePath)

    match filePath |> Path.GetExtension with
    | ".lrc" ->
        let (xmlText, meta) = contents |> xmlTextFromLyricsText
        let musicInfoText   = meta |> musicInfoTextFromMetadata
        do
          saveFileInStorage "lrc"  ".lrc" meta.Name contents
          saveFileInStorage "xml"  ".xml" meta.Name xmlText
          saveFileInStorage "info" ".txt" meta.Name musicInfoText

    | ".xml" ->
        contents
        |> lyricsTextFromXmlText |> fst
        |> printfn "%s"

    | ext ->
        failwithf "Unsupported extension: %s" ext

  let rec mainImpl argv =
    match argv |> List.ofArray with
    | [] ->
        Console.ReadLine().Split([|' '|]) |> mainImpl
    | [inputFilePath] ->
        dispatch inputFilePath
    | _ ->
        failwith "Unknown command line"

  [<EntryPoint>]
  let main argv =
    try
      mainImpl argv
      0 // exit code
    with
    | e ->
        Console.Error.Write(e.Message)
        Console.Error.Flush()
        1
