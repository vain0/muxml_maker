namespace Muxml

[<AutoOpen>]
module Lyrics =
  /// 省略されたタイムタグを前後のタイムタグで補完する
  let inline try_complete_time_tags (ottl: OptionallyTimeTaggedList<'a>): TimeTaggedList<'a> =
      assert (ottl |> List.isEmpty |> not)

      // xs に、前後のタグの情報を付加したもの
      let zipped =
          let prev_r_tags =
              (Some (TimeTag 0)) :: (ottl |> List.map (fun (_, _, r_tag) -> r_tag) |> List.dropLast)

          let next_l_tags =
              match ottl |> List.tryLast with
              | Some (_, _, end_r_tag) ->
                  List.append
                    (ottl |> List.tail |> List.map (fun (_, l_tag, _) -> l_tag))
                    [end_r_tag]
              | None -> []

          List.zip3 ottl prev_r_tags next_l_tags

      [ for ((line, l_tag_opt, r_tag_opt), prev_r_tag_opt, next_l_tag_opt) in zipped do
          let l_tag =
            match (prev_r_tag_opt, l_tag_opt) with
            | _, Some tag
            | Some tag, None -> tag
            | None, None -> failwithf "No begin time tag: %s" (string line)

          let r_tag =
            match (r_tag_opt, next_l_tag_opt) with
            | Some tag, _ | None, Some tag -> tag
            | None, None -> failwithf "No end time tag: %s" (string line)

          if l_tag >= r_tag
          then failwithf "Non-positive interval: %s" (string line)

          yield (line, l_tag, r_tag)
        ]
