namespace Muxml

open System
open System.Text
open System.Xml

[<AutoOpen>]
module XmlGen =
  // config
  let xml_offset =
    config.XmlOffset
    |> tap (fun n -> assert (n >= 0))

  let xml_from_lyrics (ls: IntervalList<_>) =
        let sh_words    = StringBuilder()
        let in_words    = StringBuilder()
        let intervals   = StringBuilder()

        // 先頭インターバルの挿入
        assert (xml_offset >= 0)
        let ls =
            match ls with
            | (None, Interval first_interval) :: ls_tail ->
                (None, Interval (xml_offset + first_interval)) :: ls_tail
            | (Some _, _) :: tail
              when xml_offset > 0 ->
                (None, Interval xml_offset) :: ls
            | _ -> ls

        for (line_opt, Interval interval) in ls do
          let line =
              match line_opt with
              | Some line -> line
              | None -> LyricsLine.Empty

          sh_words .AppendLine(sprintf "<nihongoword>%s</nihongoword>" (line.Show)) |> ignore
          in_words .AppendLine(sprintf "<word>%s</word>" (line.Input)) |> ignore
          intervals.AppendLine(sprintf "<interval>%d</interval>" (interval)) |> ignore

        let len = ls |> List.length

        ("<saidaimondaisuu>" + (string len) + "</saidaimondaisuu>" + Environment.NewLine)
        + (string sh_words)
        + (string in_words)
        + (string intervals)

  let to_xml (data: MetaData) (lyrics: Lyrics) =
    let enclose_or_empty l r = function
      | Some s -> l + s + r
      | None -> ""

    let lyrics = lyrics |> Lyrics.to_interval

    //let kpm_elem = "<kpm>" + ModelKPM.ToString("f2") + "</kpm>"

    ( "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n"
    + "<musicXML>\n"
    + "<musicname>" + data.Name + "</musicname>\n"
    + (data.MusicPath |> sprintf "<music src=\"%s\" />")
    + (data.VideoPath |> enclose_or_empty "<video src=\"" "\" scalemode=\"fullwidth\" />\n")
    + (data.PicPath   |> enclose_or_empty "<background id=\"" "\" />\n")
    + (data.Artist    |> enclose_or_empty "<argist>" "</artist>\n")
    + (data.Genre     |> enclose_or_empty "<genre>"  "</genre>\n")
    + (lyrics         |> xml_from_lyrics)
    + "</musicXML>\n"
    )

  let rec xml_text_from_lrc_text =
    function
    | HalfLyricsText lrc_text ->
        lrc_text
        |> InputLyrics.run
        |> (fun (LyricsText contents) -> contents |> xml_text_from_lrc_text)

    | FullLyricsText lrc_text ->
        match lrc_text |> Parser.parse_full_lrc with
        | Parser.MyFailure err -> failwith err
        | Parser.MySuccess (lyr, meta) ->
            let intervals = lyr |> Lyrics.to_interval |> WithInterval
            in to_xml meta intervals

    | Invalid -> failwith "Invalid lrc text."

  let lyrics_from_xml (xml: XmlNode): IntervalList<_> =
    let len     =
      xml.SelectSingleNode("saidaimondaisuu").InnerText
      |> int
    let sh_words    = xml.SelectNodes("nihongoword")
    let in_words    = xml.SelectNodes("word")
    let intervals   = xml.SelectNodes("interval")
    in
      [
        for i in 0..(len - 1) do
          let line =
            {
              Show    = sh_words.[i].InnerText
              Input   = in_words.[i].InnerText
            }
          let interval =
            intervals.[i].InnerText |> int |> Interval
          yield
            if line = LyricsLine.Empty
            then (None, interval)
            else (Some line, interval)
        ]

  let try_parse_xml (xml: XmlDocument) =
    try
      let xml = xml.SelectSingleNode("musicXML")
      let getTextElem tagName =
        xml
        |> Xml.trySelectFirst tagName
        |> Option.map (fun node -> node.InnerText)
      let getAttr tagName attrName =
        xml
        |> Xml.trySelectFirst tagName
        |> Option.map (fun node ->
            let attr = node.Attributes.GetNamedItem(attrName)
            in attr.InnerText
            )

      let lyrics =
        xml |> lyrics_from_xml |> Lyrics.WithInterval
      let meta =
        {
          Name        = xml.SelectSingleNode("musicname").InnerText
          MusicPath   = getAttr "music"       "src" |> Option.get
          PicPath     = getAttr "background"  "id"
          VideoPath   = getAttr "video"       "src"
          Artist      = getTextElem "artist"
          Genre       = getTextElem "genre"
        }
      in
        (meta, lyrics) |> Some
    with
    | _ -> None

  let lrc_text_from_xml_text (xml_text: string) =
    let xml =
      Xml.XmlDocument()
      |> tap (fun xml -> xml.LoadXml(xml_text))
    in
      match xml |> try_parse_xml with
      | Some (meta, lrc) ->
          let (LyricsText lrc_text) = Parser.unparse_full_lrc meta lrc
          in lrc_text
      | None -> failwith "failed"
