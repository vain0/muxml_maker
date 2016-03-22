namespace Muxml

open System
open System.IO

module Program =

  let dispatch file_path =
    let contents =
        File.ReadAllText(file_path)

    match file_path |> Path.GetExtension with
    | ".lrc" ->
        match
          contents
          |> Lyrics.of_string<string, TimeTag>
          |> Parser.parse_full_lrc
          with
        | Parser.MySuccess (lyr, meta) ->
            let intervals = lyr |> Lyrics.to_interval |> WithInterval
            let xml = to_xml meta intervals
            printfn "%s" xml
        | Parser.MyFailure err -> failwith err
    | ".xml" ->
        let xml =
          Xml.XmlDocument()
          |> tap (fun xml -> xml.LoadXml(contents))
        in
          match try_parse_xml xml with
          | Some (meta, lrc) ->
              let (LyricsText lrc_text) = Parser.unparse_full_lrc meta lrc
              printfn "%s" lrc_text
          | None -> failwith "failed"
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
          |> Lyrics.of_string<unit, unit>
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
