open Solvers.Signature
(* open Utils.List_utils *)

module Solver : Solver = struct
  (* type element = Mapa of mapa | Datoteka of datoteka | Nic
  and mapa = { imem : string; starsm : element; otroci : element list }
  and datoteka = { imed : string; starsd : element; velikost : int }

  type cd = Gor | Koren | V of string
  type ukaz = CD of cd | LS

  let rec velikost (el : element) =
    match el with
    | Datoteka { imed = _; starsd = _; velikost } -> velikost
    | Mapa { imem = _; starsm = _; otroci } ->
        List.fold_left (fun acc otrok -> acc + velikost otrok) 0 otroci
    | Nic -> failwith "napaka"

  let rec dodaj_otroka (trenutna_mapa: mapa) otrok =
    { trenutna_mapa with otroci = otrok :: trenutna_mapa.otroci }

  let pretvori_izhod_v_otroke (trenutna_mapa: mapa) izhod =
    izhod
    |> List.map (fun x ->
           let vrstica = x |> String.split_on_char ' ' in
           match vrstica with
           | [ "dir"; ime ] ->
               Mapa { imem = ime; starsm = Mapa trenutna_mapa; otroci = [] }
           | [ velikost; ime ] ->
               Datoteka
                 {
                   imed = ime;
                   starsd = Mapa trenutna_mapa;
                   velikost = int_of_string velikost;
                 }
           | _ -> failwith "napaka")

  let posodobi_mapo (trenutna_mapa: mapa) izhod =
    let otroci = pretvori_izhod_v_otroke trenutna_mapa izhod in
    List.fold_left
      (fun acc otrok -> dodaj_otroka acc otrok)
      trenutna_mapa otroci

  let zgradi_drevo sklopi =
    let koren = { imem = "koren"; starsm = Nic; otroci = [] } in
    List.fold_left
      (fun (trenutna_mapa: mapa) sklop ->
        let ukaz, izhod = sklop in
        match ukaz with
        | CD Gor -> koren
        | CD Koren -> koren
        | CD (V i) ->
            let zadetek = (List.find (
              fun x -> 
                match x with
                | Mapa m -> m.imem = i
                | _ -> false
            ) trenutna_mapa.otroci) in 
            (match zadetek with
            | Mapa m -> m
            | _ -> failwith "napaka")
        | LS -> (posodobi_mapo trenutna_mapa izhod) 
      )
      koren sklopi

  let sestej_velikosti (drevo : element) =
    let rec aux (drevo : element) (acc : int) =
      match drevo with
      | Datoteka d -> if d.velikost <= 100000 then d.velikost else acc
      | Mapa { imem = _; starsm = _; otroci } ->
          if velikost drevo <= 10000 then
            List.fold_left
              (fun ac otrok -> ac + aux otrok acc)
              (velikost drevo) otroci
          else List.fold_left (fun ac otrok -> ac + aux otrok acc) 0 otroci
      | Nic -> failwith "napaka"
    in
    aux drevo 0 *)

  let naloga1 (input : string) = input
    (* let poparsaj_ukaz ukaz =
      let ukaz = String.trim ukaz |> String.split_on_char ' ' in
      match ukaz with
      | [ "cd"; parameter ] -> (
          match parameter with
          | ".." -> CD Gor
          | "/" -> CD Koren
          | x -> CD (V x))
      | [ "ls" ] -> LS
      | _ -> failwith "napaka: nepodprt ukaz"
    in
    let poparsaj_sklop sklop =
      match sklop with
      | ukaz :: izhod -> (poparsaj_ukaz ukaz, izhod)
      | _ -> failwith "napaka"
    in
    let input =
      input |> String.split_on_char '$'
      |> List.map (String.split_on_char '\n')
      |> List.map (List.filter (fun x -> not (x = "")))
      |> List.filter (fun x -> List.length x != 0)
      |> List.map poparsaj_sklop |> zgradi_drevo
    in
    let _, drevo = input in
    sestej_velikosti drevo |> string_of_int *)

  let naloga2 (data : string) (_ : string) = data
end
