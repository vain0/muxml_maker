[<AutoOpen>]
module Util

open Basis.Core

module Option =
  let appendWith f l r =
      match (l, r) with
      | (Some l, Some r) -> Some (f l r)
      | ((Some _ as x), None)
      | (None, (Some _ as x)) -> x
      | (None, None) -> None

module List =
  let rec tryLast = function
    | [] -> None
    | [x] -> Some x
    | x :: xs -> tryLast xs

  let rec dropLast = function
    | [] | [_] -> []
    | x :: xs -> x :: dropLast xs
