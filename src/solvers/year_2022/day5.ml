open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  (*[B]             [B] [S]        
    [M]             [P] [L] [B] [J]
    [D]     [R]     [V] [D] [Q] [D]
    [T] [R] [Z]     [H] [H] [G] [C]
    [P] [W] [J] [B] [J] [F] [J] [S]
[N] [S] [Z] [V] [M] [N] [Z] [F] [M]
[W] [Z] [H] [D] [H] [G] [Q] [S] [W]
[B] [L] [Q] [W] [S] [L] [J] [W] [Z]
 1   2   3   4   5   6   7   8   9 *)
  let stolpci = [
    ["N";"W";"B"];
    ["B";"M";"D";"T";"P";"S";"Z";"L"];
    ["R";"W";"Z";"H";"Q"];
    ["R";"Z";"J";"V";"D";"W"];
    ["B";"M";"H";"S"];
    ["B";"P";"V";"H";"J";"N";"G";"L"];
    ["S";"L";"D";"H";"F";"Z";"Q";"J"];
    ["B";"Q";"G";"J";"F";"S";"W"];
    ["J";"D";"C";"S";"M";"W";"Z"]
    ]

  let prestavi stolpec1 stolpec2 k =
    let rec pomozna s1 s2 = function
      | 0 -> s1, s2
      | k -> pomozna (List.tl s1) (List.hd s1 :: s2) (k-1)
    in pomozna stolpec1 stolpec2 k

  let rec get k (sez)= match sez with
    | [] -> failwith "Prekratek seznam"
    | x :: xs -> if k <= 0 then x else get (k-1) xs

  let set_elem (l : string list list) (i : int) (x : string list) =
    List.mapi (fun i' el -> if i = i' then x else el) l

  let uporabi_vrstico (stolpci : string list list) (vrstica : int list) =
    match vrstica with
    | [st_premaknjenih;iz;na] -> 
      let x, y = prestavi (get (iz - 1) stolpci) (get (na - 1) stolpci) st_premaknjenih
      in
      set_elem (set_elem stolpci (iz-1) x) (na-1) y
    | _ -> failwith "Napaka"

  let is_digit = function '0' .. '9' -> true | _ -> false

  let char_to_int n =
    match n with
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | _ -> failwith "Napaka"

  let get_integers_from_string s =
    let rec pomozna acc s = 
      if String.length s > 1 then 
        match s with
        | "" -> acc
        | s -> if is_digit s.[0] then
            (if is_digit s.[1] then
              pomozna ((char_to_int (s.[0]) * 10 + char_to_int (s.[1])) :: acc) (String.sub s 2 (String.length s - 2))
            else
              pomozna ((char_to_int s.[0]) :: acc) (String.sub s 1 (String.length s - 1))) 
          else pomozna acc (String.sub s 1 (String.length s - 1))
      else
        match s with
        | "" -> acc
        | s -> if is_digit s.[0] then
            pomozna ((char_to_int s.[0]) :: acc) ""
          else pomozna acc ""
    in pomozna [] s

  let prvi_elementi (stolpci: string list list) = 
    List.fold_left (fun acc x -> acc ^ match x with | el::_ -> el | _ -> "") "" stolpci

  let rec rotiran sez = 
    match sez with
    | [] -> []
    | x :: xs -> rotiran xs @ [x]

  let naloga1 (data : string) =
    let razporedi s data =
      data
      |> String.split_on_char '\n'
      |> List.map String.trim
      |> List.map get_integers_from_string
      |> List.map rotiran
      |> (List.fold_left uporabi_vrstico s)
    in (razporedi stolpci data) |> prvi_elementi

  let rec firstk k xs = match xs with
    | [] -> []
    | x::xs -> if k=1 then [x] else x::firstk (k-1) xs

  let lastk k xs =
    xs |> rotiran |> (firstk k) |> rotiran

  let prestavi2 s1 s2 k =
    let del = firstk k s1 in
    let ostalo = lastk (List.length s1 - k) s1 in
    ostalo, del@s2

  let uporabi_vrstico2 (stolpci : string list list) (vrstica : int list) =
    match vrstica with
    | [st_premaknjenih;iz;na] -> 
      let x, y = prestavi2 (get (iz - 1) stolpci) (get (na - 1) stolpci) st_premaknjenih
      in
      set_elem (set_elem stolpci (iz-1) x) (na-1) y
    | _ -> failwith "Napaka"
  
  let naloga2 (data : string) (_ : string) =
    let razporedi s data =
      data
      |> String.split_on_char '\n'
      |> List.map String.trim
      |> List.map get_integers_from_string
      |> List.map rotiran
      |> (List.fold_left uporabi_vrstico2 s)
    in (razporedi stolpci data) |> prvi_elementi
end
   