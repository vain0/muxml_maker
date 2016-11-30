namespace MuxmlMaker

[<AutoOpen>]
module Lyrics =
  let marginThreshold =
    config.MarginThreshold

  let marginRatio =
    (config.MarginRatio.Prev, config.MarginRatio.Next)

  /// Wrap lrc string in LyricsText
  let ofString<'TInput> (content: string): LyricsText<'TInput> =
    LyricsText content

  // タイムタグつきリスト ttl の、各要素に前後のタグの時刻を付加したリスト
  /// opt: Some or id
  let pairwise opt ttl =
    assert (ttl |> List.isEmpty |> not)
    let (_, _, endRTag) = ttl |> Seq.last // Use assumption
    in
      ttl
      |> List.zipPrevNext
        (opt (TimeTag 0)) (endRTag)
        (fun (_, _, rTag) -> rTag)
        (fun (_, lTag, _) -> lTag)
      
  /// 省略されたタイムタグを前後のタイムタグで補完する
  let inline tryCompleteTimeTags
    (ottl: OptionallyTimeTaggedList<'a>): TimeTaggedList<'a>
    =
    assert (ottl |> List.isEmpty |> not)

    // ottl に、前後のタグの情報を付加したもの
    let ottlPrevNext = ottl |> pairwise Some

    in
      [ for (taggedLine, prevRTag, nextLTag) in ottlPrevNext do
          let (line, lTag, rTag) = taggedLine
          let lTag =
            match (prevRTag, lTag) with
            | _, Some tag
            | Some tag, None -> tag
            | None, None -> failwithf "No begin time tag: %s" (string line)

          let rTag =
            match (rTag, nextLTag) with
            | Some tag, _ | None, Some tag -> tag
            | None, None -> failwithf "No end time tag: %s" (string line)

          if lTag >= rTag
          then failwithf "Non-positive interval: %s" (string line)

          yield (line, lTag, rTag)
        ]

  let toInterval = function
    | WithInterval self -> self
    | WithTimeTag ttl ->
        let divideMargin = 
          let (t, h) = marginRatio
          let r = float t / float (t + h)
          let f m =
            if m < marginThreshold then
              let tMargin = m |> float |> (*) r |> round |> int
              in (tMargin, m - tMargin)
            else (0, 0)
          in f
        in
          [
            for it in ttl |> pairwise id do
              let (currentLine, TimeTag previousTime, TimeTag nextTime) = it
              let (line, TimeTag lTag, TimeTag rTag) = currentLine

              /// 行間の時間差
              let previousMargin = lTag - previousTime
              let nextMargin = nextTime - rTag

              // 行間の時間差を処理
              if previousMargin >= marginThreshold
              then yield (None, Interval previousMargin)

              let lTag = lTag - (previousMargin |> divideMargin |> snd)
              let rTag = rTag + (nextMargin |> divideMargin |> fst)

              yield (Some line, Interval (rTag - lTag))
            ]

  let toTimeTagged = function
    | WithTimeTag ttl -> ttl
    | WithInterval self ->
        self
        |> List.fold (fun (acc, total) (line, Interval interval) ->
            let total' = total + interval
            let acc' =
              match line with
              | None -> acc  // not emit gap line
              | Some line -> (line, TimeTag total, TimeTag total') :: acc
            in
              (acc', total')
            ) ([], 0)
        |> fst
        |> List.rev
