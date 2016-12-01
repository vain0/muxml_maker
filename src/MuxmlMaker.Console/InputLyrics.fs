namespace MuxmlMaker.Console

open System
open System.Text
open System.IO
open Basis.Core
open MuxmlMaker

module InputLyrics =
  type Command<'a> =
    {
      Char:
        char
      Manu:
        string
      Func:
        'a -> bool
    }
  with
    override this.ToString() =
      sprintf ":%c %s"
        (this.Char) (this.Manu)

  type CommandSet<'a> =
    {
      Description:
        string
      CommandMap:
        Map<char, Command<'a>>
    }
  with
    member this.Usage() =
      let commands =
        this.CommandMap
        |> Map.toList
        |> List.map (fun (ch, cmd) -> "  " + string cmd)
      [this.Description; "Commands:"] @ commands
      |> Str.join (Environment.NewLine)

    member this.Trigger(ch) =
      this.CommandMap
      |> Map.tryFind ch
      |> Option.getOr (this.CommandMap |> Map.find '?')
      |> (fun cmd -> cmd.Func)

  type Reader (lyrics: TimeTaggedList<string>) =
    let shows =
      lyrics |> List.map (fun (show, _, _) -> show) |> Array.ofList

    let length = shows.Length

    let inputs =
      Array.create length None

    let mutable index = 0

  with
    member this.Lyrics          = lyrics
    member this.Shows           = shows
    member this.Inputs          = inputs
    member this.Index           = index
    member this.Length          = length

    member this.MoveToIfAble(newIndex) =
      if 0 <= newIndex && newIndex < length
      then index <- newIndex

    static member MoveToIfAbleImpl newIndex (this: Reader) =
      this.MoveToIfAble(newIndex)

    static member TryMoveToUnreadableLine ixs (this: Reader) =
      ixs
      |> Seq.tryFind
        (fun i ->
          0 <= i && i < this.Length
          && this.Inputs.[i] = None
        )
      |> (function
          | Some i ->
            this.MoveToIfAble(i)
            true
          | None -> false
          )

    static member TryBuild (this: Reader) =
      Option.if' (this.Inputs |> Array.forall (Option.isSome))
        (fun () ->
          (this.Lyrics, (this.Shows |> List.ofArray), (this.Inputs |> List.ofArray))
          |||> List.zip3
          |> List.map
            (fun ((_, l, r), show, input) ->
              assert (input |> Option.isSome)
              ({ Show = show; Input = input |> Option.get }, l, r)
            ))

    static member EditCurrentLine (this: Reader) =
      let (orig, _, _) = this.Lyrics.[this.Index]
      printfn "%s" "Input new line. (Empty or \"@\" to pass)"
      printfn "#%3d %s" (this.Index) orig
      let cur = &(this.Shows.[this.Index])
      if cur <> orig then
        printfn "#CUR %s" cur
      match Console.ReadLine() with
      | null | "" | "@" -> ()
      | show -> cur <- show
      true

    static member CommandMap =
      {
        Description = "Input how to type for each line."
        CommandMap =
          [ { Char = '?'
              Manu = "Show this usage"
              Func = (fun _ -> printfn "%s" (Reader.CommandMap.Usage()); true)
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
                  if this |> Reader.TryMoveToUnreadableLine ixs |> not then
                    printfn "%s" "Not found."
                  true)
              }
            { Char = 'f'
              Manu = "Skip to next unreadable line"
              Func = (fun (this: Reader) ->
                  let ixs = [(this.Index + 1) .. (this.Length - 1)]
                  if this |> Reader.TryMoveToUnreadableLine ixs |> not then
                    printfn "%s" "Not found."
                  true)
              }
            { Char = 'n'
              Manu = "Edit current line"
              Func = Reader.EditCurrentLine
              }
          ]
          |> List.map (fun cmd -> (cmd.Char, cmd))
          |> Map.ofList
      }

    static member ShowUsage() =
      printfn "%s" (Reader.CommandMap.Usage())

    static member ProcessCommand ch this =
      this |> Reader.CommandMap.Trigger(ch)

    static member ShowCurrentLine (this: Reader) =
      printfn "#%3d %s" this.Index this.Shows.[this.Index]

      this.Inputs.[this.Index]
      |> Option.iter (fun s -> printfn "#CUR %s" s)

    static member ReadAll (this: Reader) =
      let (|Command|Input|) line =
        if line |> Str.isNullOrEmpty || line = ":" then
          Command '?'
        else if line.[0] = ':' then
          Command line.[1]
        else
          Input line

      let rec loop () =
        this |> Reader.ShowCurrentLine

        match Console.ReadLine() with
        | Command ch ->
          if this |> Reader.ProcessCommand ch then
            loop ()

        | Input line ->
          this.Inputs.[this.Index] <- Some line
          if this.Index < this.Length - 1
            then
              this.MoveToIfAble (this.Index + 1)
              loop ()
            else
              if this |> Reader.TryMoveToUnreadableLine [0..(this.Length - 1)]
              then loop ()
              else
                // TODO: 一覧を出力して、本当に完了するか聞く
                () // 入力終了
      loop ()

    member this.Run() =
      Reader.ShowUsage()
      this |> Reader.ReadAll
      this |> Reader.TryBuild

  let run (contents: LyricsText<unit>) =
    match contents |> Parser.parseHalfLyrics with
    | Parser.MySuccess (lyr, _) ->
      let reader = Reader(lyr |> Lyrics.toTimeTagged)
      match reader.Run() with
      | None ->
        failwith "Incomplete"
      | Some lyr ->
        // TODO: メタデータを付加すべき
        let sb = StringBuilder()
        for (line, lTag, rTag) in lyr do
          sb.AppendLine(sprintf "%s%s%s" (string lTag) line.Show (string rTag))
            .AppendLine(line.Input) |> ignore
        string sb
        |> Lyrics.ofString<string>

    | Parser.MyFailure msg -> failwith msg
