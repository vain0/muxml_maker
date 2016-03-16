namespace Muxml

[<AutoOpen>]
module Lyrics =
  // config
  let margin_threshold = 300
  let margin_ratio = (5, 1)

  // タイムタグつきリスト ttl の、各要素に前後のタグの時刻を付加したリスト
  /// opt: Some or id
  let with_prev_next_tags opt ttl =
      assert (ttl |> List.isEmpty |> not)
      let (_, _, end_r_tag) = ttl |> Seq.last // Use assumption
      ttl
      |> List.zipPrevNext
        (opt (TimeTag 0)) (end_r_tag)
        (fun (_, _, r_tag) -> r_tag)
        (fun (_, l_tag, _) -> l_tag)
      
  /// 省略されたタイムタグを前後のタイムタグで補完する
  let inline try_complete_time_tags (ottl: OptionallyTimeTaggedList<'a>): TimeTaggedList<'a> =
      assert (ottl |> List.isEmpty |> not)

      // ottl に、前後のタグの情報を付加したもの
      let ottlPrevNext = ottl |> with_prev_next_tags Some

      [ for ((line, l_tag_opt, r_tag_opt), prev_r_tag_opt, next_l_tag_opt) in ottlPrevNext do
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
    | WithTimeTag ttl ->
        let divide_margin = 
            let (t, h) = margin_ratio
            let r = float t / float (t + h)
            fun m ->
                if m < margin_threshold then
                  let t_margin = m |> float |> (*) r |> round |> int
                  (t_margin, m - t_margin)
                else (0, 0)

        [ for it in ttl |> with_prev_next_tags id do
            let (cur_line, TimeTag prev_time, TimeTag next_time) = it
            let (line, TimeTag l_tag, TimeTag r_tag) = cur_line

            /// 行間の時間差
            let prev_margin = l_tag - prev_time
            let next_margin = next_time - r_tag

            // 行間の時間差を処理
            if prev_margin >= margin_threshold
            then yield (None, Interval prev_margin)

            let l_tag = l_tag - (prev_margin |> divide_margin |> snd)
            let r_tag = r_tag + (next_margin |> divide_margin |> fst)

            yield (Some line, Interval (r_tag - l_tag))
          ]

  let to_time_tagged = function
    | WithTimeTag ttl -> ttl
    | WithInterval self ->
        failwith "Unimplemented"
