namespace Muxml

open System
open System.Text
open System.IO
open Basis.Core

module InputLyrics =
  let show_usage () =
      printfn """
Input how to type for each line.
Commands:
  :?  Show this
  :q  Quit (No save)
  :k  Back to the previous line
  :j  Skip to the next line
"""

  type Reader (lyr_: TimeTaggedList<string>) =
      let shows_ =
          lyr_ |> List.map (fun (show, _, _) -> show) |> Array.ofList

      let len_ = shows_.Length

      let inputs_ =
          Array.create len_ None

      let mutable index_ = 0

  with
    member this.Lyrics  = lyr_
    member this.Shows   = shows_
    member this.Inputs  = inputs_
    member this.Index   = index_
    member this.Length  = len_

    member this.MoveToIfAble(new_index) =
        if 0 <= new_index && new_index < len_
        then index_ <- new_index

    static member move_to_if_able new_index (this: Reader) =
        this.MoveToIfAble(new_index)

    static member try_build (this: Reader) =
        Option.if' (this.Inputs |> Array.forall (Option.isSome)) (fun () ->
          (this.Lyrics, (this.Inputs |> List.ofArray))
          ||> List.zip
          |> List.map (fun ((show, l, r), input) ->
              assert (input |> Option.isSome)
              ({ Show = show; Input = input |> Option.get }, l, r)
              )
          )

    static member proc_command ch (this: Reader) =
        match ch with
        | 'k' ->
            this.MoveToIfAble (this.Index - 1)
            true
        | 'j' ->
            this.MoveToIfAble (this.Index + 1)
            true
        | 'q' ->
            printfn "Quit? (Y/n)"
            Console.ReadLine() <> "Y"
        | '?' | _ -> 
            show_usage ()
            true

    static member show_current_line (this: Reader) =
        printfn "#%3d %s" (this.Index) (this.Shows.[this.Index])

        this.Inputs.[this.Index]
        |> Option.iter (fun s -> printfn "#CUR %s" s)

    static member read_all (this: Reader) =
        let (|Command|Input|) line =
            if line |> Str.isNullOrEmpty || line = ":" then
              Command '?'
            else if line.[0] = ':' then
              Command line.[1]
            else
              Input line

        let rec loop () =
            this |> Reader.show_current_line

            match Console.ReadLine() with
            | Command ch ->
                if this |> Reader.proc_command ch
                then loop ()

            | Input line ->
                this.Inputs.[this.Index] <- Some line
                if this.Index < this.Length - 1
                then
                  this.MoveToIfAble (this.Index + 1)
                  loop ()
                else
                  // TODO: 未入力のものがあれば戻って聞き直す
                  // TODO: 一覧を出力して、本当に完了するか聞く
                  // 入力終了
                  ()
        loop ()

    member this.Run() =
        show_usage ()
        this |> Reader.read_all
        this |> Reader.try_build

  let from_file path =
      assert (path |> Path.GetExtension = ".lrc")
      let contents =
          File.ReadAllText(path)

      match contents |> Parser.parse_half_lrc with
      | Parser.MySuccess (lyr, _) ->
          let reader = Reader(lyr |> Lyrics.to_time_tagged)
          match reader.Run() with
          | None ->
              failwith "Incomplete"
          | Some lyr ->
              // TODO: メタデータを付加すべき
              let sb = StringBuilder()
              for (line, l_tag, r_tag) in lyr do
                  sb.AppendLine(sprintf "%s%s%s" (string l_tag) line.Show (string r_tag))
                    .AppendLine(line.Input) |> ignore
              printfn "%s" (string sb)

      | Parser.MyFailure msg -> failwith msg
