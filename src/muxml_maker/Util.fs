[<AutoOpen>]
module Util

module List =
  // not used
  let rec tryLast = function
    | [] -> None
    | [x] -> Some x
    | x :: xs -> tryLast xs

  let rec dropLast = function
    | [] | [_] -> []
    | x :: xs -> x :: dropLast xs

  /// xs に、各要素の前後の情報を加えたリスト
  let zipPrevNext front back prev next xs =
      let prevs = front :: (xs |> dropLast |> List.map prev)
      let nexts = (xs |> List.tail |> List.map next) @ [back]
      List.zip3 xs prevs nexts
