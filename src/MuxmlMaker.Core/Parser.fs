namespace MuxmlMaker

open System
open Basis.Core
open FParsec

module Parser =
  type MyResult<'r, 'e> =
    | MySuccess of 'r
    | MyFailure of 'e

  module LrcParser =
    type LrcConfig = Map<string, string>

    let initialState = Map.empty

    let runState cfg: MetaData =
      let name =
        match cfg |> Map.tryFind "name" with
        | Some name -> name
        | None -> ""
      {
        Name =
          name
        MusicPath =
          match cfg |> Map.tryFind "music" with
          | Some path -> path
          | None -> name + ".mp3"
        PicPath =
          cfg |> Map.tryFind "pic"
        VideoPath =
          cfg |> Map.tryFind "video"
        Artist =
          cfg |> Map.tryFind "artist"
        Genre =
          cfg |> Map.tryFind "genre"
      }

    let inline runResult pr =
      match pr with
      | ParserResult.Success (xs, state, pos) ->
        try
          let ttl = Lyrics.tryCompleteTimeTags xs
          MySuccess (ttl |> WithTimeTag, runState state)
        with
        | e -> MyFailure e.Message
      | ParserResult.Failure (msg, error, state) ->
        MyFailure msg

    type Parser<'a> =
      Parser<'a, LrcConfig>

    let skip p = p >>% ()
    
    // ``@ key = value`` adds config to (key: value) pair
    let pMetaLine: Parser<_> =
      parse {
        do! skipChar '@' .>> spaces
        let! key    = many1Chars asciiLetter
        do! spaces >>. skipChar '=' .>> spaces
        let! value  = restOfLine true
        do! updateUserState (Map.add key value)
      }

    let pNewLine: Parser<_> =
      skipNewline >>. many (attempt pMetaLine <|> skipNewline)

    let pOneLine =
      restOfLine false .>> pNewLine

    let pTimeTag: Parser<_> =
      let body =
        pipe3 (pint32 .>> skipChar ':') (pint32 .>> skipChar ':') pint32
          (fun min sec ms10 -> TimeTag <| ((((min * 60) + sec) * 100) + ms10) * 10)
      between (skipChar '[') (skipChar ']') body

    let pLTag = opt (attempt pTimeTag)
    let pRTag = opt (attempt pTimeTag) .>> pNewLine

    let pTimeTaggedLine =
      parse {
        let! lTag = pLTag
        let! line  = manyCharsTill anyChar (followedBy (attempt (skip pTimeTag) <|> skipNewline))
        let! rTag = pRTag
        return (line, lTag, rTag)
      }

    let pTimeTaggedLineAndInputLyrics: Parser<_> =
      pipe2
        pTimeTaggedLine
        pOneLine
        (fun (show, lTag, rTag) input ->
          ({ Show = show; Input = input }, lTag, rTag)
        )

    let pEof =
      spaces >>. eof

    let pHalfLyrics =
      skipMany (attempt pMetaLine)
      >>. many pTimeTaggedLine
      .>> pEof

    let pFullLyrics =
      skipMany (attempt pMetaLine)
      >>. many pTimeTaggedLineAndInputLyrics
      .>> pEof

  let parseHalfLyricsImpl contents =
    runParserOnString
      LrcParser.pHalfLyrics
      LrcParser.initialState
      "lrc-half parser" (contents + "\n")

  /// .lrc (with time tags, w/o input lyrics)
  let parseHalfLyrics (LyricsText contents: LyricsText<unit>) =
    parseHalfLyricsImpl contents
    |> LrcParser.runResult

  /// .lrc (with time tags and input lyrics)
  let parseFullLyrics (LyricsText contents: LyricsText<string>) =
    let result =
      runParserOnString
        (LrcParser.pFullLyrics) (LrcParser.initialState)
        "lrc-full parser" (contents + "\n")
    result |> LrcParser.runResult

  let unparseLyricsMetadata (meta: MetaData) =
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

  let unparseHalfLyrics meta (lrc: UnreadableLyrics) =
    lrc
    |> Lyrics.toTimeTagged
    |> List.fold
      (fun acc (line, TimeTag lTag, TimeTag rTag) ->
        (string lTag + line + string rTag) :: acc
      ) []
    |> List.rev
    |> Str.join Environment.NewLine
    |> (+) (unparseLyricsMetadata meta)
    |> Lyrics.ofString<unit>

  let unparseFullLyrics meta (lrc: ReadableLyrics) =
    lrc
    |> Lyrics.toTimeTagged
    |> List.fold
      (fun acc (line, lTag, rTag) ->
        let { Show = show; Input = input } = line
        input :: (string lTag + show + string rTag) :: acc
      ) []
    |> List.rev
    |> Str.join Environment.NewLine
    |> (+) (unparseLyricsMetadata meta)
    |> Lyrics.ofString<string>

[<AutoOpen>]
module LyricsExtension =
  /// .lrc テキストの判別
  /// 偶数行にタイムタグがあり、奇数行にない場合、full テキストらしさが高いと判定する。
  let (|HalfLyricsText|FullLyricsText|Invalid|) contents =
    match Parser.parseHalfLyricsImpl contents with
    | Success (ottl, state, pos) ->
      let indicator =
        function
        | Some _ -> 1
        | None -> 0
      let prob =
        ottl
        |> List.mapi
          (fun i (line, lTag, rTag) ->
            if i % 2 = 0
              then      indicator lTag  +      indicator rTag
              else (1 - indicator lTag) + (1 - indicator rTag)
            |> float
          )
        |> List.sum
        |> flip (/) (ottl |> List.length |> (*) 2 |> float)
      if prob > 0.7
        then FullLyricsText (contents |> Lyrics.ofString<string>)
        else HalfLyricsText (contents |> Lyrics.ofString<unit  >)

    | Failure _ ->
        Invalid
