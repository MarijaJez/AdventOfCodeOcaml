open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let je_vsebovan (par_parov : (int * int) * (int * int)) =
    let par1, par2 = par_parov in
    let a, b = par1 in
    let c, d = par2 in
    if (a >= c && b <= d) || (a <= c && b >= d) then true else false

  let input_v_sez_sez (vrstica : string) =
    vrstica |> Str.split (Str.regexp ",") |> List.map (String.split_on_char '-')

  let sez_sez_v_par_parov (sez : string list list) =
    match sez with
    | [ [ a; b ]; [ c; d ] ] ->
        ((int_of_string a, int_of_string b), (int_of_string c, int_of_string d))
    | _ -> failwith "napaka"

  let prestej (sez : bool list) =
    let rec prestej' sez acc =
      match sez with
      | [] -> acc
      | h :: t -> if h then prestej' t (acc + 1) else prestej' t acc
    in
    prestej' sez 0

  let naloga1 (data : string) =
    data |> String.split_on_char '\n' |> List.map String.trim
    |> List.map input_v_sez_sez
    |> List.map sez_sez_v_par_parov
    |> List.map je_vsebovan |> prestej |> string_of_int

  let se_prekriva (par_parov : (int * int) * (int * int)) =
    let par1, par2 = par_parov in
    let a, b = par1 in
    let c, d = par2 in
    if (a >= c && a <= d) || (a <= c && c <= b) || je_vsebovan par_parov then
      true
    else false

  (* join two strings *)
  let naloga2 (data : string) (_ : string) =
    data |> String.split_on_char '\n' |> List.map String.trim
    |> List.map input_v_sez_sez
    |> List.map sez_sez_v_par_parov
    |> List.map se_prekriva |> prestej |> string_of_int
end
