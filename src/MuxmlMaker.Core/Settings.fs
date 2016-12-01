namespace MuxmlMaker

open System
open System.IO
open FSharp.Configuration

[<AutoOpen>]
module Settings =
  type Config = YamlConfig<"MuxmlMaker.yaml">
  let config = Config()

  let storagePath =
    let path = config.StoragePath
    if path |> Directory.Exists
      then Some path
      else None
