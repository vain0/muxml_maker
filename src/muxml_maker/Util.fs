[<AutoOpen>]
module Util

module List =
  let rec tryLast = function
    | [] -> None
    | [x] -> Some x
    | x :: xs -> tryLast xs
