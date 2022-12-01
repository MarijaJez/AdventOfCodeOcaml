let preberi_datoteko ime_datoteke =
  let chan = open_in_bin ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina
in
let vsebina_datoteke = preberi_datoteko "data/2022/day_1.in" in
let load vsebina_datoteke =
  let s = String.split_on_char '\n' vsebina_datoteke in
  s |> List.map String.trim |> List.map int_of_string
in
let sez = load vsebina_datoteke in
List.iter print_endline sez
