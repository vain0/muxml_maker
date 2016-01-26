namespace Muxml

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
    let p_meta_line: Parser<_> = parse {
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

        between (skipChar '[') (skipChar ']') body

    let p_l_tag = opt (attempt p_time_tag)
    let p_r_tag = opt (attempt p_time_tag) .>> p_newline

    let p_time_tagged_line = parse {
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
        optional (attempt p_meta_line)
        >>. many p_time_tagged_line
        .>> p_eof

    let p_full_lrc =
        optional (attempt p_meta_line)
        >>. many p_time_tagged_line_and_input_lyrics
        .>> p_eof
        
  /// .lrc (with time tags, w/o input lyrics)
  let parse_half_lrc contents =
      runParserOnString
        (LrcParser.p_half_lrc) (LrcParser.init_state)
        "lrc-half parser" (contents + "\n")
      |> LrcParser.run_result

  /// .lrc (with time tags and input lyrics)
  let parse_full_lrc contents =
      let result =
          runParserOnString
            (LrcParser.p_full_lrc) (LrcParser.init_state)
            "lrc-full parser" (contents + "\n")
      result |> LrcParser.run_result
