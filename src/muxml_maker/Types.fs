namespace Muxml

[<AutoOpen>]
module Types =
  type LyricsLine = {
      // 表示歌詞
      Show:  string
      // 入力歌詞
      Input: string
  }
  with
    static member Empty =
        { Show = "@"; Input = "@" }
    
    override this.ToString() =
        if this = LyricsLine.Empty
        then "@"
        else sprintf "%s[%s]" (this.Show) (this.Input)

  type TimeTag =
    | TimeTag of int

  type Interval =
    | Interval of int

  type OptionallyTimeTaggedList<'a> =
      ('a * TimeTag option * TimeTag option) list

  type TimeTaggedList<'a> =
      ('a * TimeTag * TimeTag) list

  type IntervalList<'a> =
      ('a * Interval) list

  type LyricsRepr<'a> =
    | WithTimeTag     of TimeTaggedList<'a>
    | WithInterval    of IntervalList<'a option>

  // 入力歌詞が未設定な歌詞データ
  type UnreadableLyrics =
      LyricsRepr<string>

  type Lyrics =
      LyricsRepr<LyricsLine>

  type MetaData = {
      Name            : string
      MusicPath       : string
      PicPath         : string option
      VideoPath       : string option
      Artist          : string option
      Genre           : string option
  }
