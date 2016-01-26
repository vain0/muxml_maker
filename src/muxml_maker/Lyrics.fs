﻿namespace Muxml

[<AutoOpen>]
module Lyrics =
  // config
  let margin_threshold = 300
  let margin_ratio = (5, 1)

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
  
  let to_interval = function
    | WithInterval self -> self
    | WithTimeTag ls ->
        // ls に、次行の開始時刻を付加したもの
        let zipped_ls =
            List.zip ls
              (match ls |> List.tryLast with
              | Some (_, _, TimeTag end_time) ->
                  let l_tags =
                    ls |> List.tail |> List.map (fun (_, TimeTag time, _) -> time)
                  List.append l_tags [end_time]
              | None -> []
              )

        /// 前行との間の時間差で、今の行の前方に組み込まれる時間
        let h_margin = ref 0

        [ for (cur_line, next_time) in zipped_ls do
            let (line, TimeTag l_tag, TimeTag r_tag) = cur_line
            let l_tag = l_tag + (! h_margin)
            h_margin := 0

            /// 次行との時間差
            let margin = next_time - r_tag

            // 行間の時間差を処理
            if margin >= margin_threshold then
              yield (Some line, Interval (r_tag - l_tag))
              yield (None, Interval margin)
            else
              // 前後の行に分割して加える
              let (t, h) = margin_ratio
              let t_margin   = margin * t / (t + h)
              h_margin      := margin - t_margin
              yield (Some line, Interval (r_tag - l_tag + t_margin))
          ]
