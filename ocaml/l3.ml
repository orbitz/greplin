#use "topfind";;
#thread;;
#require "dynlink";;
#camlp4o;;
#require "core_extended";;
#require "seq";;

open Core_extended.Std;;
open Core_extended.Function;;

let (|>) d f = f d;;

let numbers = [3; 4; 9; 14; 15; 19; 28; 37; 47; 50; 54; 56; 59; 61; 70; 73; 78; 81; 92; 95; 97; 99];;

let all_subsets l =
  let rec non_empty_subsets = function
    | [] -> []
    | x::xs -> let f ys r = ys :: (x :: ys) :: r in
	       [x] :: List.fold_right ~f:f ~init:[] (non_empty_subsets xs)
  in
  [] :: non_empty_subsets l;;

let numbers_ss = all_subsets numbers;;


let series =
  numbers_ss
  |> Seq.of_list
  |> Seq.filter ~f:(function | [] -> false | [_] -> false | _ -> true)
  |> Seq.map ~f:(fun x -> List.fold_left ~f:(+) ~init:0 x)
  |> Seq.filter ~f:(fun x -> List.mem x numbers)
  |> Seq.to_list;;

let total = List.length series;;
