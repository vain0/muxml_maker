[<AutoOpen>]
module Util

module List =
  let rec tryLast = function
    | [] -> None
    | [x] -> Some x
    | x :: xs -> tryLast xs

  let rec dropLast = function
    | [] | [_] -> []
    | x :: xs -> x :: dropLast xs

  let zipPrevNext front back xs =
      let prevs = front :: (xs |> dropLast)
      let nexts = (xs |> List.tail) @ [back]
      List.zip3 xs front back
