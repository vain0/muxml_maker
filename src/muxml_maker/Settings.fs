﻿namespace Muxml

open System
open System.IO
open FSharp.Configuration

[<AutoOpen>]
module Settings =
  type Config = YamlConfig<"muxml_maker.yaml">
  let config = Config()

  let storage_path =
    let path = config.StoragePath
    if path |> Directory.Exists
    then Some path
    else None
