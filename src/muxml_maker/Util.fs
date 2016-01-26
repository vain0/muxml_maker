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

  // 1回の走査で (len, sum, min, max) の4つを計算する
  let lenSumMinMax add xs =
      List.fold (fun (len, acc, mi, ma) x ->
          ( len + 1
          , (acc, Some x) ||> Option.appendWith add
          , (mi,  Some x) ||> Option.appendWith min
          , (ma,  Some x) ||> Option.appendWith max
          )) (0, None, None, None) xs

  let inline trimMean xs =
      let (|Or0|) = Option.getOr (LanguagePrimitives.GenericZero)
      let (len, Or0 sum, Or0 mi, Or0 ma) = xs |> lenSumMinMax (+)
      let sum =
          if len <= 2 then sum else sum - mi - ma
      (sum, len) ||> LanguagePrimitives.DivideByInt
