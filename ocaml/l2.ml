#use "topfind";;
#thread;;
#require "dynlink";;
#camlp4o;;
#require "core_extended";;
#require "seq";;

open Core_extended.Std;;
open Core_extended.Function;;

let (|>) d f = f d;;

let fib () = 
  let rec fib' x y = [< 'x + y; fib' y (x + y) >] in
  fib' 1 1;;

let is_prime n =
  let sn = int_of_float (sqrt (float n)) + 1 in
  let rec is_prime' p =
    if p > sn then
      true
    else if n mod p = 0 then
      false
    else
      is_prime' (p + 2)
  in
  if n = 2 then
    true
  else if n mod 2 = 0 then
    false
  else
    is_prime' 3;;

let v = fib () |> Seq.filter (fun n -> n > 227_000) |> Seq.filter is_prime |> Seq.take 1 |> Seq.to_list;;
let v = 514_229 + 1;;

let rec seq_n start = [< 'start; seq_n (start + 1) >];;

let rec prime_factors n =
  if is_prime n then
    [< 'n >]
  else
    let pf s =
      match Seq.next s with
	| Some x -> [< prime_factors x; prime_factors (n / x) >]
	| None -> [< >]
    in
    seq_n 2
    |> Seq.take (int_of_float (sqrt (float n)))
    |> Seq.filter (fun x -> n mod x = 0)
    |> pf;;

let pd = prime_factors v |> Seq.to_list;;

let sum = List.fold_left ~f:(+) ~init:0 pd;;
