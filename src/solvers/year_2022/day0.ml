open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let cost_fun x = (x / 3) - 2

  let rec full_cost x =
    let c_cost = cost_fun x in
    if c_cost <= 0 then 0 else c_cost + full_cost c_cost

  let naloga1 data =
    let lines = List.lines data in
    let rec aux = function
      | [] -> Printf.printf "]\n"
      | x :: xs ->
          Printf.printf "{%7s}, " x;
          aux xs
    in
    let () = aux lines in
    Printf.printf "%d\n" (List.length lines);
    lines |> List.int_list
    |> List.fold_left (fun s x -> s + cost_fun x) 0
    |> string_of_int

  let naloga2 data _part1 =
    data |> List.lines |> List.int_list |> List.map full_cost |> List.sum
    |> string_of_int
end
