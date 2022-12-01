open Solvers.Signature
open Utils.List_utils
open Str

(* module Solver : Solver = struct *)
module Solver = struct
  let rec vsota sez = match sez with [] -> 0 | h :: t -> h + vsota t;;
  let max_number_list lst = List.fold_left max 0 lst;;

  let naloga1 data =
    let lines = Str.split (regexp "\n \n") data in lines 
      |> List.map String.trim
      |> List.map (String.split_on_char '\n')
      |> List.map (List.map int_of_string)
      |> List.map vsota 
      |> max_number_list
      |> string_of_int

  let naloga2 (*data _part1*) = failwith "Dopolni me"
  (* data |> List.lines |> List.int_list |> List.map full_cost |> List.sum
     |> string_of_int *)
end
