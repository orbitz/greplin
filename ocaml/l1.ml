#use "topfind";;
#thread;;
#require "dynlink";;
#camlp4o;;
#require "core_extended";;
#require "seq";;

open Core_extended.Std;;
open Core_extended.Function;;
open Ort.Function;;
open Ort;;

let s = "FourscoreandsevenyearsagoourfaathersbroughtforthonthiscontainentanewnationconceivedinzLibertyanddedicatedtothepropositionthatallmenarecreatedequalNowweareengagedinagreahtcivilwartestingwhetherthatnaptionoranynartionsoconceivedandsodedicatedcanlongendureWeareqmetonagreatbattlefiemldoftzhatwarWehavecometodedicpateaportionofthatfieldasafinalrestingplaceforthosewhoheregavetheirlivesthatthatnationmightliveItisaltogetherfangandproperthatweshoulddothisButinalargersensewecannotdedicatewecannotconsecratewecannothallowthisgroundThebravelmenlivinganddeadwhostruggledherehaveconsecrateditfaraboveourpoorponwertoaddordetractTgheworldadswfilllittlenotlenorlongrememberwhatwesayherebutitcanneverforgetwhattheydidhereItisforusthelivingrathertobededicatedheretotheulnfinishedworkwhichtheywhofoughtherehavethusfarsonoblyadvancedItisratherforustobeherededicatedtothegreattdafskremainingbeforeusthatfromthesehonoreddeadwetakeincreaseddevotiontothatcauseforwhichtheygavethelastpfullmeasureofdevotionthatweherehighlyresolvethatthesedeadshallnothavediedinvainthatthisnationunsderGodshallhaveanewbirthoffreedomandthatgovernmentofthepeoplebythepeopleforthepeopleshallnotperishfromtheearth";;

let ls = String.to_list s;;

let rec all_substr s = function
  | 0 -> [< >]
  | n -> [< 'List.take s n; all_substr s (n - 1) >];;

let rec every_substr = function
  | [] -> [< >]
  | x::xs -> [< all_substr (x::xs) (List.length (x::xs)); every_substr xs >];;

let is_reverse ls = ls = List.rev ls;;

let sorted = ls
  |> every_substr
  |> Seq.filter ~f:is_reverse
  |> Seq.to_list
  |> List.sort ~cmp:(fun x y -> List.length y - List.length x);;

let password = List.hd sorted;;
