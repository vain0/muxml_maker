namespace Muxml

open System
open System.IO

module Program =

  let dispatch file_path =
    let contents =
        File.ReadAllText(file_path)

    match file_path |> Path.GetExtension with
    | ".lrc" ->
        match Parser.parse_full_lrc contents with
        | Parser.MySuccess (lyr, meta) ->
            let intervals = lyr |> Lyrics.to_interval |> WithInterval
            let xml = to_xml meta intervals
            printfn "%s" xml
        | Parser.MyFailure err -> failwith err
    | ext ->
        failwithf "Unsupported extension: %s" ext

  let rec main_impl argv =
    match argv |> List.ofArray with
    | [] ->
        Console.ReadLine().Split([|' '|]) |> main_impl
    | [input_file_path] ->
        dispatch input_file_path
    | ["--input"; input_file_path]
    | ["-i";      input_file_path] ->
        InputLyrics.from_file input_file_path
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
