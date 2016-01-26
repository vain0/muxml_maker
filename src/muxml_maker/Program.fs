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

  [<EntryPoint>]
  let main argv = 
    let input_file_path =
        if argv.Length > 0
        then argv.[0]
        else
          Console.ReadLine ()
        
    try
      dispatch input_file_path
      0 // exit code
    with
    | e ->
        Console.Error.Write(e.Message)
        Console.Error.Flush()
        1
