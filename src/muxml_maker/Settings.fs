namespace Muxml

open FSharp.Configuration

[<AutoOpen>]
module Settings =
  type Config = YamlConfig<"muxml_maker.yaml">
  let config = Config()
