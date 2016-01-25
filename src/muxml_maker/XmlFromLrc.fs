namespace Muxml

module Converter =
  module XmlFromLrc =
    type Info = { Name: string }

    ()

  open XmlFromLrc

  let xml_from_lrc (info: Info) =
    let lyrics = ""

    let kpm_elem =
        ""
        //"<kpm>" + ModelKPM.ToString("f2") + "</kpm>"

    let video_elem =
        ""
        //(Video.Uses ? "<video src=\"" + Video.GetFullPath() + "\" scalemode=\"fullwidth\" />\n" : "")

    let pic_elem =
        ""//(Pic.Uses   ? "<background id=\"" + Pic.GetFullPath() + "\" />\n" : "")

    ( "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n"
    + "<musicXML>\n"
    + video_elem
    + pic_elem
    + "<musicname>" + info.Name + "</musicname>\n"
    + lyrics
    + "</musicXML>\n"
    )
