open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let ruzak_str_to_list (s : string) =
    let rec str_to_lst i l =
      if i < 0 then l else str_to_lst (i - 1) (s.[i] :: l)
    in
    str_to_lst (String.length s - 1) []

  let rec dolzina (list : char list) =
    match list with [] -> 0 | _ :: xs -> 1 + dolzina xs

  let razdeli_ruzak_na_pol (ruzak : char list) =
    let rec divide k sez =
      match (k, sez) with
      | _, [] -> ([], [])
      | k, sez when k <= 0 -> ([], sez)
      | k, x :: xs ->
          let sez1, sez2 = divide (k - 1) xs in
          (x :: sez1, sez2)
    in
    divide (dolzina ruzak / 2) ruzak

  let poisci_skupni_element (dva_lista : char list * char list) =
    let list1, list2 = dva_lista in
    let rec pomozna list1 list2 =
      match list1 with
      | [] -> None
      | x :: xs -> if List.mem x list2 then Some x else pomozna xs list2
    in
    match pomozna list1 list2 with
    | None -> raise (Invalid_argument "string_option is None")
    | Some x -> x

  let rec vsota sez = match sez with [] -> 0 | h :: t -> h + vsota t

  let vrednost_elementa (el : char) =
    match el with
    | 'a' -> 1
    | 'b' -> 2
    | 'c' -> 3
    | 'd' -> 4
    | 'e' -> 5
    | 'f' -> 6
    | 'g' -> 7
    | 'h' -> 8
    | 'i' -> 9
    | 'j' -> 10
    | 'k' -> 11
    | 'l' -> 12
    | 'm' -> 13
    | 'n' -> 14
    | 'o' -> 15
    | 'p' -> 16
    | 'q' -> 17
    | 'r' -> 18
    | 's' -> 19
    | 't' -> 20
    | 'u' -> 21
    | 'v' -> 22
    | 'w' -> 23
    | 'x' -> 24
    | 'y' -> 25
    | 'z' -> 26
    | 'A' -> 27
    | 'B' -> 28
    | 'C' -> 29
    | 'D' -> 30
    | 'E' -> 31
    | 'F' -> 32
    | 'G' -> 33
    | 'H' -> 34
    | 'I' -> 35
    | 'J' -> 36
    | 'K' -> 37
    | 'L' -> 38
    | 'M' -> 39
    | 'N' -> 40
    | 'O' -> 41
    | 'P' -> 42
    | 'Q' -> 43
    | 'R' -> 44
    | 'S' -> 45
    | 'T' -> 46
    | 'U' -> 47
    | 'V' -> 48
    | 'W' -> 49
    | 'X' -> 50
    | 'Y' -> 51
    | 'Z' -> 52
    | _ ->
        Printf.printf "Napaka pri vrednost_elementa";
        0

  let naloga1 (data : string) =
    data |> String.split_on_char '\n' |> List.map ruzak_str_to_list
    |> List.map razdeli_ruzak_na_pol
    |> List.map poisci_skupni_element
    |> List.map vrednost_elementa |> vsota |> string_of_int

  let list_to_list_trojic (sez : string list) =
    let rec pomozna sez acc =
      match sez with
      | [] -> acc
      | x :: y :: z :: xs -> pomozna xs ((x, y, z) :: acc)
      | _ -> raise (Invalid_argument "sez je kratek")
    in
    pomozna sez []

  let poisci_skupni_element_3 (tri_stringi : string * string * string) =
    let str1, str2, str3 = tri_stringi in
    let list1, list2, list3 =
      (ruzak_str_to_list str1, ruzak_str_to_list str2, ruzak_str_to_list str3)
    in
    let rec pomozna list1 list2 list3 =
      match list1 with
      | [] -> None
      | x :: xs ->
          if List.mem x list2 && List.mem x list3 then Some x
          else pomozna xs list2 list3
    in
    match pomozna list1 list2 list3 with
    | None -> raise (Invalid_argument "string_option is None")
    | Some x -> x

  let naloga2 (data : string) (_ : string) =
    data |> String.split_on_char '\n' |> list_to_list_trojic
    |> List.map poisci_skupni_element_3
    |> List.map vrednost_elementa |> vsota |> string_of_int
end
