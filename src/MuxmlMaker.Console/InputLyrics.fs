namespace MuxmlMaker.Console

open System
open System.Text
open System.IO
open Basis.Core

module InputLyrics =
  type Command<'a> =
    {
      Char      : char
      Manu      : string
      Func      : 'a -> bool
    }
  with
    override this.ToString() =
      sprintf ":%c %s"
        (this.Char) (this.Manu)

  type CommandSet<'a> =
    {
      Description   : string
      CommandMap    : Map<char, Command<'a>>
    }
  with
    member this.Usage() =
      let commands =
        this.CommandMap
        |> Map.toList
        |> List.map (fun (ch, cmd) -> "  " + string cmd)
      in
        [this.Description; "Commands:"] @ commands
        |> Str.join (Environment.NewLine)

    member this.Trigger(ch) =
      this.CommandMap
      |> Map.tryFind ch
      |> Option.getOr (this.CommandMap |> Map.find '?')
      |> (fun cmd -> cmd.Func)

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

    static member try_move_to_unreadable_line ixs (this: Reader) =
      ixs
      |> Seq.tryFind (fun i ->
          0 <= i && i < this.Length
          && this.Inputs.[i] = None
          )
      |> (function
          | Some i -> this.MoveToIfAble(i); true
          | None -> false
          )

    static member try_build (this: Reader) =
      Option.if' (this.Inputs |> Array.forall (Option.isSome)) (fun () ->
        (this.Lyrics, (this.Shows |> List.ofArray), (this.Inputs |> List.ofArray))
        |||> List.zip3
        |> List.map (fun ((_, l, r), show, input) ->
            assert (input |> Option.isSome)
            ({ Show = show; Input = input |> Option.get }, l, r)
            )
        )

    static member edit_current_line (this: Reader) =
      let (orig, _, _) = this.Lyrics.[this.Index]
      do
        printfn "%s" "Input new line. (Empty or \"@\" to pass)"
        printfn "#%3d %s" (this.Index) orig

      let cur = &(this.Shows.[this.Index])
      if cur <> orig
      then printfn "#CUR %s" cur

      match Console.ReadLine() with
      | null | "" | "@" -> ()
      | show -> cur <- show
      
      true

    static member command_map =
      {
        Description = "Input how to type for each line."
        CommandMap =
          [ { Char = '?'
              Manu = "Show this usage"
              Func = (fun _ -> printfn "%s" (Reader.command_map.Usage()); true)
              }
            { Char = 'q'
              Manu = "Quit (no save)"
              Func = (fun _ ->
                  printfn "Quit? (Y/n)"
                  Console.ReadLine() <> "Y"
                  )
              }
            { Char = 'k'
              Manu = "Back to previous line"
              Func = (fun (this: Reader) -> this.MoveToIfAble(this.Index - 1); true)
              }
            { Char = 'j'
              Manu = "Skip to next line"
              Func = (fun (this: Reader) -> this.MoveToIfAble(this.Index + 1); true)
              }
            { Char = 'b'
              Manu = "Back to previous unreadable line"
              Func = (fun (this: Reader) ->
                  let ixs = [(this.Index - 1) .. -1 .. 0]
                  if this |> Reader.try_move_to_unreadable_line ixs |> not
                  then printfn "%s" "Not found."
                  true)
              }
            { Char = 'f'
              Manu = "Skip to next unreadable line"
              Func = (fun (this: Reader) ->
                  let ixs = [(this.Index + 1) .. (this.Length - 1)]
                  if this |> Reader.try_move_to_unreadable_line ixs |> not
                  then printfn "%s" "Not found."
                  true)
              }
            { Char = 'n'
              Manu = "Edit current line"
              Func = Reader.edit_current_line
              }
          ]
          |> List.map (fun cmd -> (cmd.Char, cmd))
          |> Map.ofList
      }

    static member show_usage() =
      printfn "%s" (Reader.command_map.Usage())

    static member proc_command ch this =
      this |> Reader.command_map.Trigger(ch)

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
              if this |> Reader.try_move_to_unreadable_line [0..(this.Length - 1)]
              then loop ()
              else
                // TODO: 一覧を出力して、本当に完了するか聞く
                () // 入力終了
      loop ()

    member this.Run() =
      Reader.show_usage()
      this |> Reader.read_all
      this |> Reader.try_build

  let run (contents: LyricsText<unit>) =
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
            string sb
            |> Lyrics.of_string<string>

    | Parser.MyFailure msg -> failwith msg
