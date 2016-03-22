namespace Muxml

open System
open System.IO
open Parser

module Program =

  let dispatch file_path =
    let contents =
        File.ReadAllText(file_path)

    match file_path |> Path.GetExtension with
    | ".lrc" ->
        contents
        |> xml_text_from_lrc_text
        |> printfn "%s"

    | ".xml" ->
        contents
        |> lrc_text_from_xml_text
        |> printfn "%s"

    | ext ->
        failwithf "Unsupported extension: %s" ext

  let rec main_impl argv =
    match argv |> List.ofArray with
    | [] ->
        Console.ReadLine().Split([|' '|]) |> main_impl
    | "input" :: argv ->
        let content =
            match argv with
            | [] -> Console.In.ReadToEnd()
            | file_path :: _ ->
                File.ReadAllText(file_path)
        let (LyricsText lrc_text) =
          content
          |> Lyrics.of_string<unit>
          |> InputLyrics.run
        in
          printf "%s" lrc_text
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
