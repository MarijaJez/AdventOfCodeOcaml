(* #use "topfind";;
   #require "str";; *)
open Solvers.Signature
open Utils.List_utils
open Str

module Solver : Solver = struct
  let rec vsota sez = match sez with [] -> 0 | h :: t -> h + vsota t
  let max_number_list lst = List.fold_left max 0 lst

  let naloga1 (data : string) =
    data
    |> Str.split (regexp "\n\n")
    |> List.map String.trim
    |> List.map (String.split_on_char '\n')
    |> List.map (List.map int_of_string)
    |> List.map vsota |> max_number_list |> string_of_int

  (* join two strings *)
  let naloga2 (data : string) (_ : string) =
    data
    |> Str.split (regexp "\n\n")
    |> List.map String.trim
    |> List.map (String.split_on_char '\n')
    |> List.map (List.map int_of_string)
    |> List.map vsota
    |> List.sort (fun a b -> b - a)
    |> List.take 3 |> vsota |> string_of_int
end
