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
