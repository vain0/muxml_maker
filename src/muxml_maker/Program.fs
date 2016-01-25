namespace Muxml

open System
open System.IO
open Converter

module Program =

  let dispatch file_path =
    let contents =
        File.ReadAllText(file_path)

    match file_path |> Path.GetExtension with
    | ".lrc" ->
        let xml_text = Converter.xml_from_lrc contents
        printfn "%s" xml_text
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
