namespace MuxmlMaker.Console

open System
open System.IO
open Basis.Core
open MuxmlMaker
open MuxmlMaker.Parser

module Program =

  let save_file_in_storage dir_name ext song_name contents =
    option {
      let! root   = storage_path
      let dir     = Path.Combine(root, dir_name)
      let path    = Path.Combine(dir, song_name + ext)
      if dir |> Directory.Exists then
        File.WriteAllText(path, contents)
    } |> ignore

  let dispatch file_path =
    let contents =
        File.ReadAllText(file_path)

    match file_path |> Path.GetExtension with
    | ".lrc" ->
        let (xml_text, meta) = contents |> xml_text_from_lrc_text
        let music_info_text   = meta |> music_info_text_from_meta
        do
          save_file_in_storage "lrc"  ".lrc" meta.Name contents
          save_file_in_storage "xml"  ".xml" meta.Name xml_text
          save_file_in_storage "info" ".txt" meta.Name music_info_text

    | ".xml" ->
        contents
        |> lrc_text_from_xml_text |> fst
        |> printfn "%s"

    | ext ->
        failwithf "Unsupported extension: %s" ext

  let rec main_impl argv =
    match argv |> List.ofArray with
    | [] ->
        Console.ReadLine().Split([|' '|]) |> main_impl
    | [input_file_path] ->
        dispatch input_file_path
    | _ ->
        failwith "Unknown command line"

  [<EntryPoint>]
  let main argv =
    try
      main_impl argv
      0 // exit code
    with
    | e ->
        Console.Error.Write(e.Message)
        Console.Error.Flush()
        1
