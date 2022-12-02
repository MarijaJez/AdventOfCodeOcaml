open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let rec vsota sez = match sez with [] -> 0 | h :: t -> h + vsota t

  let tocke_ena_igra sez =
    match sez with
    | [ "A"; "X" ] -> 1 + 3
    | [ "A"; "Y" ] -> 2 + 6
    | [ "A"; "Z" ] -> 3 + 0
    | [ "B"; "X" ] -> 1 + 0
    | [ "B"; "Y" ] -> 2 + 3
    | [ "B"; "Z" ] -> 3 + 6
    | [ "C"; "X" ] -> 1 + 6
    | [ "C"; "Y" ] -> 2 + 0
    | [ "C"; "Z" ] -> 3 + 3
    | _ ->
        Printf.printf "Napaka pri tocke_ena_igra";
        0

  let naloga1 (data : string) =
    data |> String.split_on_char '\n'
    |> List.map (String.split_on_char ' ')
    |> List.map tocke_ena_igra |> vsota |> string_of_int

  let tocke_ena_igra_1 sez =
    match sez with
    | [ "A"; "X" ] -> 3 + 0
    | [ "A"; "Y" ] -> 1 + 3
    | [ "A"; "Z" ] -> 2 + 6
    | [ "B"; "X" ] -> 1 + 0
    | [ "B"; "Y" ] -> 2 + 3
    | [ "B"; "Z" ] -> 3 + 6
    | [ "C"; "X" ] -> 2 + 0
    | [ "C"; "Y" ] -> 3 + 3
    | [ "C"; "Z" ] -> 1 + 6
    | _ ->
        Printf.printf "Napaka pri tocke_ena_igra";
        0

  let naloga2 (data : string) (_ : string) =
    data |> String.split_on_char '\n'
    |> List.map (String.split_on_char ' ')
    |> List.map tocke_ena_igra_1 |> vsota |> string_of_int
end
