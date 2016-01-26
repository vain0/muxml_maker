namespace Muxml

open System
open System.Text

[<AutoOpen>]
module XmlGen =
  // config
  let xml_offset = 200 // >= 0

  let xml_from_lyrics = function
    | WithInterval ls ->
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

          sh_words .AppendLine(sprintf "<word>%s</word>" (line.Show)) |> ignore
          in_words .AppendLine(sprintf "<nihongoword>%s</nihongoword>" (line.Input)) |> ignore
          intervals.AppendLine(sprintf "<interval>%d</interval>" (interval)) |> ignore

        let len = ls |> List.length

        ("<saidaimondaisuu>" + (string len) + "</saidaimondaisuu>" + Environment.NewLine)
        + (string sh_words)
        + (string in_words)
        + (string intervals)
    | _ ->
        failwith "Unimplemented"

  let to_xml (data: MetaData) (lyrics: Lyrics) =
    let enclose_or_empty l r = function
      | Some s -> l + s + r
      | None -> ""

    //let kpm_elem = "<kpm>" + ModelKPM.ToString("f2") + "</kpm>"

    ( "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n"
    + "<musicXML>\n"
    + "<musicname>" + data.Name + "</musicname>\n"
    + (data.VideoPath |> enclose_or_empty "<video src=\"" "\" scalemode=\"fullwidth\" />\n")
    + (data.PicPath   |> enclose_or_empty "<background id=\"" "\" />\n")
    + (data.Artist    |> enclose_or_empty "<argist>" "</artist>\n")
    + (data.Genre     |> enclose_or_empty "<genre>"  "</genre>\n")
    + (lyrics         |> xml_from_lyrics)
    + "</musicXML>\n"
    )
