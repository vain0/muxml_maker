namespace MuxmlMaker.Console

open System
open System.Security
open System.Text
open System.Xml
open MuxmlMaker

[<AutoOpen>]
module XmlGen =
  // config
  let xmlOffset =
    config.XmlOffset
    |> tap (fun n -> assert (n >= 0))

  let xmlFromLyrics (ls: IntervalList<_>) =
    let showWords    = StringBuilder()
    let inputWords    = StringBuilder()
    let intervals   = StringBuilder()

    // 先頭インターバルの挿入
    assert (xmlOffset >= 0)
    let ls =
      match ls with
      | (None, Interval firstInterval) :: lsTail ->
        (None, Interval (xmlOffset + firstInterval)) :: lsTail
      | (Some _, _) :: tail
        when xmlOffset > 0 ->
        (None, Interval xmlOffset) :: ls
      | _ -> ls

    for (line, Interval interval) in ls do
      let line =
        match line with
        | Some line -> line
        | None -> LyricsLine.Empty
      let showWordsFormat =
        sprintf "<nihongoword>%s</nihongoword>"
      let inputWordsFormat =
        sprintf "<word>%s</word>"
      let intervalsFormat =
        sprintf "<interval>%d</interval>" 
      showWords.AppendLine(showWordsFormat line.Show) |> ignore
      inputWords.AppendLine(inputWordsFormat line.Input) |> ignore
      intervals.AppendLine(intervalsFormat interval) |> ignore

    let len = ls |> List.length
    ( "<saidaimondaisuu>" + (string len) + "</saidaimondaisuu>"
    + Environment.NewLine
    + (string showWords)
    + (string inputWords)
    + (string intervals)
    )

  let toXml (data: MetaData) (lyrics: Lyrics) =
    let lyrics = lyrics |> Lyrics.toInterval

    //let kpmElem = "<kpm>" + ModelKPM.ToString("f2") + "</kpm>"

    ( "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n"
    + "<musicXML>\n"
    + "<musicname>" + data.Name + "</musicname>\n"
    + (data.MusicPath |> sprintf "<music src=\"%s\" />\n")
    + (data.VideoPath |> encloseOrEmpty "<video src=\"" "\" scalemode=\"fullwidth\" />\n")
    + (data.PicPath   |> encloseOrEmpty "<background id=\"" "\" />\n")
    + (data.Artist    |> encloseOrEmpty "<argist>" "</artist>\n")
    + (data.Genre     |> encloseOrEmpty "<genre>"  "</genre>\n")
    + (lyrics         |> xmlFromLyrics)
    + "</musicXML>\n"
    )

  let musicInfoTextFromMetadata (meta: MetaData) =
    ( "<musicinfo>\n"
    + "  <musicname>" + SecurityElement.Escape(meta.Name) + "</musicname>\n"
    + (meta.Artist  |> encloseOrEmpty "  <artist>" "</artist>\n")
    + (meta.Genre   |> encloseOrEmpty "  <genre>"  "</genre>\n")
    + "</musicinfo>\n"
    )

  let rec xmlTextFromLyricsText =
    function
    | HalfLyricsText lyricsText ->
      lyricsText
      |> InputLyrics.run
      |> (fun (LyricsText contents) -> contents |> xmlTextFromLyricsText)
    | FullLyricsText lyricsText ->
      match lyricsText |> Parser.parseFullLyrics with
      | Parser.MyFailure err -> failwith err
      | Parser.MySuccess (lyr, meta) ->
        let intervals = lyr |> Lyrics.toInterval |> WithInterval
        (toXml meta intervals, meta)
    | Invalid -> failwith "Invalid lrc text."

  let lyricsFromXml (xml: XmlNode): IntervalList<_> =
    let len =
      xml.SelectSingleNode("saidaimondaisuu").InnerText
      |> int
    let showWords =
      xml.SelectNodes("nihongoword")
    let inputWords =
      xml.SelectNodes("word")
    let intervals =
      xml.SelectNodes("interval")
    [
      for i in 0..(len - 1) do
        let line =
          {
            Show =
              showWords.[i].InnerText
            Input =
              inputWords.[i].InnerText
          }
        let interval =
          intervals.[i].InnerText |> int |> Interval
        yield
          if line = LyricsLine.Empty
            then (None, interval)
            else (Some line, interval)
      ]

  let tryParseXml (xml: XmlDocument) =
    try
      let xml = xml.SelectSingleNode("musicXML")
      let getTextElem tagName =
        xml
        |> Xml.trySelectFirst tagName
        |> Option.map (fun node -> node.InnerText)
      let getAttr tagName attrName =
        xml
        |> Xml.trySelectFirst tagName
        |> Option.map
          (fun node ->
            let attr = node.Attributes.GetNamedItem(attrName)
            attr.InnerText
          )

      let lyrics =
        xml |> lyricsFromXml |> Lyrics.WithInterval
      let meta =
        {
          Name =
            xml.SelectSingleNode("musicname").InnerText
          MusicPath =
            getAttr "music"       "src" |> Option.get
          PicPath =
            getAttr "background"  "id"
          VideoPath =
            getAttr "video"       "src"
          Artist =
            getTextElem "artist"
          Genre =
            getTextElem "genre"
        }
      in
        (meta, lyrics) |> Some
    with
    | _ -> None

  let lyricsTextFromXmlText (xmlText: string) =
    let xml =
      Xml.XmlDocument()
      |> tap (fun xml -> xml.LoadXml(xmlText))
    match xml |> tryParseXml with
    | Some (meta, lrc) ->
      let (LyricsText lyricsText) = Parser.unparseFullLyrics meta lrc
      (lyricsText, meta)
    | None -> failwith "failed"
