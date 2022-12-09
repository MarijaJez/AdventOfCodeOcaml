open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct

  let is_bigger_than_left ints =
    let rec is_bigger_than_left_helper remaining_ints max_so_far =
      match remaining_ints with
      | [] -> []
      | h::t -> (h > max_so_far) :: (is_bigger_than_left_helper t (max h max_so_far))
    in
    is_bigger_than_left_helper ints min_int

  let vidna_drevesa_z_leve (gozd : int list list) : bool list list =
    let rec vidna_drevesa_z_leve_helper gozd =
      match gozd with
      | [] -> []
      | h::t -> (is_bigger_than_left h) :: (vidna_drevesa_z_leve_helper t)
    in
    vidna_drevesa_z_leve_helper gozd

  let zasukaj matrika =
    let rec zasukaj_helper gozd =
      match gozd with
      | [] -> []
      | h::t -> (List.rev h) :: (zasukaj_helper t)
    in
    zasukaj_helper matrika

  let vidna_drevesa_z_desne (gozd : int list list) : bool list list =
    gozd |> zasukaj |> vidna_drevesa_z_leve |> zasukaj

  let transponiraj matrika =
    let rec transpose_helper int_lists transposed_int_lists =
      match int_lists with
      | [] -> transposed_int_lists
      | h::t -> transpose_helper t (List.map2 (fun x y -> x::y) h transposed_int_lists)
    in
    (transpose_helper matrika (List.map (fun _ -> []) (List.hd matrika))) |> zasukaj
    

  let vidna_drevesa_od_zgoraj (gozd : int list list) : bool list list =
    gozd |> transponiraj |> vidna_drevesa_z_leve |> transponiraj

  let vidna_drevesa_od_spodaj (gozd : int list list) : bool list list =
    gozd |> transponiraj |> vidna_drevesa_z_desne |> transponiraj

  let vidna_drevesa_v_gozdu (gozd : int list list) : bool list list =
    let levo, desno, gor, dol = vidna_drevesa_z_leve gozd, vidna_drevesa_z_desne gozd, vidna_drevesa_od_zgoraj gozd, vidna_drevesa_od_spodaj gozd in
    let rec vidna_drevesa_v_gozdu_helper levo desno gor dol =
      match levo, desno, gor, dol with
      | [], [], [], [] -> []
      | h1::t1, h2::t2, h3::t3, h4::t4 -> (List.map2 (fun x y -> x || y) (List.map2 (fun x y -> x || y) h1 h2) (List.map2 (fun x y -> x || y) h3 h4)) :: (vidna_drevesa_v_gozdu_helper t1 t2 t3 t4)
      | _ -> failwith "Napaka: nekatere matrike niso enake dolzine."
    in
    vidna_drevesa_v_gozdu_helper levo desno gor dol
  
  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []
  
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
    | _ -> failwith "Napaka: ni int"

  let prestej (vrstica : bool list) : int = 
    let rec pomozna vrstica acc =
      match vrstica with
      | [] -> acc
      | x :: xs -> if x = true then pomozna xs (acc + 1) else pomozna xs acc
    in pomozna vrstica 0

  let prestej_vidne (gozd: bool list list) : int = 
      let rec pomozna gozd acc =
        match gozd with 
        | [] -> acc
        | vrsta :: gozd' -> pomozna gozd' (acc + prestej vrsta)
      in pomozna gozd 0
    
  let naloga1 (data : string) =
    data |> String.split_on_char '\n'
    |> List.map (explode)
    |> List.map (List.map char_to_int)
    |> vidna_drevesa_v_gozdu
    |> prestej_vidne |> string_of_int

  let vidna_desno (vrsta: int list) : int list =
    let rec vidna_desno_na_mestu mesto desni acc =
      match desni with
      | [] -> acc
      | d :: ds -> if d >= mesto then (acc + 1) else vidna_desno_na_mestu mesto ds (acc+1)
    in 
    let rec pomozna vrsta acc =
      match vrsta with
      | [] -> acc
      | x :: xs -> ((vidna_desno_na_mestu x xs 0) :: pomozna xs acc) 
    in pomozna vrsta []

  let vidna_levo (vrsta : int list) : int list =
    vrsta |> List.rev |> vidna_desno |> List.rev

  let vidna_na_desno (gozd: int list list) : int list list =
    gozd |> List.map vidna_desno

  let vidna_na_levo (gozd: int list list) : int list list =
    gozd |> List.map vidna_levo

  let vidna_na_gor (gozd : int list list) : int list list =
    gozd |> transponiraj |> vidna_na_levo |> transponiraj

  let vidna_na_dol (gozd : int list list) : int list list =
    gozd |> transponiraj |> vidna_na_desno |> transponiraj

  (* let vidna_drevesa_na_mestu (gozd : int list list) : int list list =
    let levo, desno, gor, dol = vidna_na_levo gozd, vidna_na_desno gozd, vidna_na_gor gozd, vidna_na_dol gozd in
    let rec vidna_drevesa_v_gozdu_helper levo desno gor dol =
      match levo, desno, gor, dol with
      | [], [], [], [] -> []
      | h1::t1, h2::t2, h3::t3, h4::t4 -> 
        (List.map2 (fun x y -> x * y) (List.map2 (fun x y -> x * y) h1 h2) (List.map2 (fun x y -> x * y) h3 h4)) :: (vidna_drevesa_v_gozdu_helper t1 t2 t3 t4)
      | _ -> failwith "Napaka: nekatere matrike niso enake dolzine."
    in
    vidna_drevesa_v_gozdu_helper levo desno gor dol *)

    let vidna_drevesa_na_mestu (gozd : int list list) : int list =
      let levo, desno, gor, dol = List.flatten (vidna_na_levo gozd), (List.flatten (vidna_na_desno gozd)), (List.flatten (vidna_na_gor gozd)), (List.flatten (vidna_na_dol gozd)) in
      let rec pomozna levo desno gor dol acc =
        match levo, desno, gor, dol with
        | [], [], [], [] -> acc
        | h1::t1, h2::t2, h3::t3, h4::t4 -> pomozna t1 t2 t3 t4 ((h1 * h2 * h3 * h4) :: acc)
        | _ -> failwith "Napaka: nekatere matrike niso enake dolzine."
      in
      pomozna levo desno gor dol []
      
  let max int_list = List.fold_left (fun max x -> if x > max then x else max) min_int int_list

  (* let najvecji (gozd : int list list) : int = 
    gozd |> List.map max |> max *)

  let naloga2 (data : string) (_ : string) =
    data |> String.split_on_char '\n'
    |> List.map (explode)
    |> List.map (List.map char_to_int)
    |> vidna_drevesa_na_mestu
    |> max |> string_of_int
end
