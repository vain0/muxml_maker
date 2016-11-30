namespace MuxmlMaker.Console

open System
open Basis.Core
open FParsec

module Parser =
  type MyResult<'r, 'e> =
    | MySuccess of 'r
    | MyFailure of 'e

  module LrcParser =
    type LrcConfig = Map<string, string>

    let init_state = Map.empty

    let run_state cfg =
      let name =
        match cfg |> Map.tryFind "name" with
        | Some name -> name
        | None -> ""
      in
        {
          MetaData.Name = name
          MusicPath   =
            match cfg |> Map.tryFind "music" with
            | Some path -> path
            | None -> name + ".mp3"
          PicPath     = cfg |> Map.tryFind "pic"
          VideoPath   = cfg |> Map.tryFind "video"
          Artist      = cfg |> Map.tryFind "artist"
          Genre       = cfg |> Map.tryFind "genre"
        }

    let inline run_result pr =
      match pr with
      | ParserResult.Success (xs, state, pos) ->
          try
            let ttl = try_complete_time_tags xs
            MySuccess (ttl |> WithTimeTag, run_state state)
          with
          | e -> MyFailure e.Message
      | ParserResult.Failure (msg, error, state) ->
          MyFailure msg

    type Parser<'a> = Parser<'a, LrcConfig>

    let skip p = p >>% ()
    
    // ``@ key = value`` adds config to (key: value) pair
    let p_meta_line: Parser<_> =
      parse {
        do! skipChar '@' .>> spaces
        let! key    = many1Chars asciiLetter
        do! spaces >>. skipChar '=' .>> spaces
        let! value  = restOfLine true
        do! updateUserState (Map.add key value)
      }

    let p_newline: Parser<_> =
      skipNewline >>. many (attempt p_meta_line <|> skipNewline)

    let p_one_line =
      restOfLine false .>> p_newline

    let p_time_tag: Parser<_> =
      let body =
        pipe3 (pint32 .>> skipChar ':') (pint32 .>> skipChar ':') pint32
          (fun min sec ms10 -> TimeTag <| ((((min * 60) + sec) * 100) + ms10) * 10)
      in
        between (skipChar '[') (skipChar ']') body

    let p_l_tag = opt (attempt p_time_tag)
    let p_r_tag = opt (attempt p_time_tag) .>> p_newline

    let p_time_tagged_line =
      parse {
        let! l_tag = p_l_tag
        let! line  = manyCharsTill anyChar (followedBy (attempt (skip p_time_tag) <|> skipNewline))
        let! r_tag = p_r_tag
        return (line, l_tag, r_tag)
      }

    let p_time_tagged_line_and_input_lyrics: Parser<_> =
      pipe2
        p_time_tagged_line
        p_one_line
        (fun (show, l_tag, r_tag) input ->
            ({ Show = show; Input = input }, l_tag, r_tag)
            )

    let p_eof =
      spaces >>. eof

    let p_half_lrc =
      skipMany (attempt p_meta_line)
      >>. many p_time_tagged_line
      .>> p_eof

    let p_full_lrc =
      skipMany (attempt p_meta_line)
      >>. many p_time_tagged_line_and_input_lyrics
      .>> p_eof

  let parse_half_lrc_impl contents =
    runParserOnString
      (LrcParser.p_half_lrc) (LrcParser.init_state)
      "lrc-half parser" (contents + "\n")

  /// .lrc (with time tags, w/o input lyrics)
  let parse_half_lrc (LyricsText contents: LyricsText<unit>) =
    parse_half_lrc_impl contents
    |> LrcParser.run_result

  /// .lrc (with time tags and input lyrics)
  let parse_full_lrc (LyricsText contents: LyricsText<string>) =
    let result =
      runParserOnString
        (LrcParser.p_full_lrc) (LrcParser.init_state)
        "lrc-full parser" (contents + "\n")
    in
      result |> LrcParser.run_result

  let unparse_lrc_meta_header (meta: MetaData) =
    [
      "@name = "  + meta.Name       |> Some
      "@music = " + meta.MusicPath  |> Some
      meta.PicPath    |> Option.map ((+) "@pic = ")
      meta.VideoPath  |> Option.map ((+) "@video = ")
      meta.Artist     |> Option.map ((+) "@artist = ")
      meta.Genre      |> Option.map ((+) "@genre = ")
    ]
    |> List.choose id
    |> Str.join Environment.NewLine

  let unparse_half_lrc meta (lrc: UnreadableLyrics) =
    lrc
    |> Lyrics.to_time_tagged
    |> List.fold (fun acc (line, TimeTag l_tag, TimeTag r_tag) ->
        (string l_tag + line + string r_tag) :: acc
        ) []
    |> List.rev
    |> Str.join Environment.NewLine
    |> (+) (unparse_lrc_meta_header meta)
    |> Lyrics.of_string<unit>

  let unparse_full_lrc meta (lrc: Lyrics) =
    lrc
    |> Lyrics.to_time_tagged
    |> List.fold (fun acc (line, l_tag, r_tag) ->
        let { Show = show; Input = input } = line
        in
          input :: (string l_tag + show + string r_tag) :: acc
        ) []
    |> List.rev
    |> Str.join Environment.NewLine
    |> (+) (unparse_lrc_meta_header meta)
    |> Lyrics.of_string<string>

[<AutoOpen>]
module LyricsExtension =
  /// .lrc テキストの判別
  /// 偶数行にタイムタグがあり、奇数行にない場合、full テキストらしさが高いと判定する。
  let (|HalfLyricsText|FullLyricsText|Invalid|) contents =
    match Parser.parse_half_lrc_impl contents with
    | Success (ottl, state, pos) ->
        let ind =
          function
          | Some _ -> 1
          | None -> 0
        let prob =
          ottl
          |> List.mapi (fun i (line, l_tag, r_tag) ->
              if i % 2 = 0
              then      ind l_tag  +      ind r_tag
              else (1 - ind l_tag) + (1 - ind r_tag)
              |> float
              )
          |> List.sum
          |> flip (/) (ottl |> List.length |> (*) 2 |> float)
        in
          if prob > 0.7
          then FullLyricsText (contents |> Lyrics.of_string<string>)
          else HalfLyricsText (contents |> Lyrics.of_string<unit  >)

    | Failure _ ->
        Invalid
