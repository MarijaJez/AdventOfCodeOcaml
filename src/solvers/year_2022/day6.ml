open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let string_to_char_list (str : string) =
    let rec exp i l = if i < 0 then l else exp (i - 1) (str.[i] :: l) in
    exp (String.length str - 1) []

  let remove_last_list_item list = List.rev (List.tl (List.rev list))

  let rec diverse_list_items list =
    match list with
    | [] -> true
    | x :: xs -> if List.mem x xs then false else diverse_list_items xs

  let poisci_prve_stiri_razlicne (niz : string) =
    let znaki = string_to_char_list niz in
    let rec pomozna (niz : char list) (acc : char list) (st : int) =
      match niz with
      | [] -> st
      | x :: xs ->
          if st < 4 then pomozna xs (x :: acc) (st + 1)
          else if diverse_list_items acc && not (List.mem x acc) then st
          else pomozna xs (x :: remove_last_list_item acc) (st + 1)
    in
    pomozna znaki [] 0

  let naloga1 (data : string) =
    data |> poisci_prve_stiri_razlicne |> string_of_int

  let poisci_prvih_stirinajst_razlicnih (niz : string) =
    let znaki = string_to_char_list niz in
    let rec pomozna (niz : char list) (acc : char list) (st : int) =
      match niz with
      | [] -> st
      | x :: xs ->
          if st < 14 then pomozna xs (x :: acc) (st + 1)
          else if diverse_list_items acc then st
          else pomozna xs (x :: remove_last_list_item acc) (st + 1)
    in
    pomozna znaki [] 0

  let naloga2 (data : string) (_ : string) =
    data |> poisci_prvih_stirinajst_razlicnih |> string_of_int
end
