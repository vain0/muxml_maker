namespace Muxml

open System
open System.Text
open System.IO
open Basis.Core

module InputLyrics =
  type State = {
      Lyrics          : TimeTaggedList<string>
      Shows           : string []
      Inputs          : string option []
      mutable Index   : int
  }
  with
    static member Empty (lyr: TimeTaggedList<string>) =
        let shows =
            lyr |> List.map (fun (show, _, _) -> show) |> Array.ofList
        {
          Lyrics    = lyr
          Shows     = shows
          Inputs    = Array.create (shows.Length) None
          Index     = 0
        }

    member this.MovePrevIfAble() =
        if this.Index > 0
        then this.Index <- this.Index - 1

    member this.MoveNextIfAble() =
        if this.Index < (this.Inputs.Length - 1)
        then this.Index <- this.Index + 1

    member this.IsAtLast =
        this.Index = this.Inputs.Length - 1

    member this.IsComplete =
        this.Inputs |> Array.forall (Option.isSome)

    static member try_build (this: State) =
        Option.if' (this.IsComplete) (fun () ->
          (this.Lyrics, (this.Inputs |> List.ofArray))
          ||> List.zip
          |> List.map (fun ((show, l, r), input) ->
              assert (input |> Option.isSome)
              ({ Show = show; Input = input |> Option.get }, l, r)
              )
          )

  let show_usage () =
      printfn """
Input how to type for each line.
Commands:
  :?  Show this
  :q  Quit (No save)
  :k  Back to the previous line
  :j  Skip to the next line
"""

  let proc_command c (state: State) kont =
      match c with
      | 'k' ->
          state.MovePrevIfAble()
          state |> kont
      | 'j' ->
          state.MoveNextIfAble()
          state |> kont
      | 'q' ->
          printfn "Quit? (Y/n)"
          if Console.ReadLine() = "Y"
          then state
          else state |> kont
      | '?' | _ -> 
          show_usage ()
          state |> kont

  let rec ask state =
      printfn "#%3d %s" (state.Index) (state.Shows.[state.Index])

      state.Inputs.[state.Index]
      |> Option.iter (fun s -> printfn "#CUR %s" s)

      let line =
          Console.ReadLine()
          |> (fun s -> if s |> Str.isNullOrEmpty || s = ":" then ":?" else s)

      if line.[0] = ':'
      then (state, ask) ||> proc_command (line.[1])
      else
        state.Inputs.[state.Index] <- Some line
        if state.IsAtLast
        then
            // TODO: 未入力のものがあれば戻って聞き直す
            // TODO: 一覧を出力して、本当に完了するか聞く
            state
        else
            state.MoveNextIfAble()
            state |> ask

  let ask_all lyr =
      show_usage ()
      State.Empty lyr
      |> ask
      |> State.try_build

  let from_file path =
      assert (path |> Path.GetExtension = ".lrc")
      let contents =
          File.ReadAllText(path)

      match contents |> Parser.parse_half_lrc with
      | Parser.MySuccess (lyr, _) ->
          match lyr |> Lyrics.to_time_tagged |> ask_all with
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
