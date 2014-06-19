
open ExtLib

let print_db db =
  Hashtbl.iter begin fun first db' ->
    Hashtbl.iter begin fun second count ->
      Printf.printf "%s -> %s: %d\n" first second count
    end db'
  end db

let process text =
  let db = Hashtbl.create 100 in
  let populate first second =
    try 
      let db' = Hashtbl.find db first in
      try
        let x = Hashtbl.find db' second in
        Hashtbl.replace db' second (x + 1)
      with (Not_found) ->
        Hashtbl.add db' second 1
    with (Not_found) ->
      let db' = Hashtbl.create 10 in
      Hashtbl.add db' second 1;
      Hashtbl.add db first db'
  in
  let process words =
    let words = Array.of_list words in
    let length = Array.length words in
    Array.iteri begin fun i word ->
      let (first, second) = word, (if i < (length - 1) then words.(i + 1) else ".") in
      populate first second
      (* Printf.printf "%s * %s\n" first second *)
    end words
  in
  let split_sentence sentence =
    let sentence = String.strip sentence in
    let rex = Pcre.regexp ~flags:[`UTF8] "(?:[^\\pL\\pN.])+" in
    let words = Pcre.split ~rex sentence in
    process words
  in
  let split_line line =
    let sentences = Pcre.split ~pat:"\\." line in
    List.iter split_sentence sentences
  in
  List.iter split_line text;
  db

let generate db =
  let rnd = Random.int (Hashtbl.length db) in
  let first = (Array.of_list (List.of_enum (Hashtbl.keys db))).(rnd) in
  let generate_next word =
    let db' = Hashtbl.find db word in
    let total = Hashtbl.fold (fun _second count acc -> acc + count) db' 0 in
    let rnd = Random.int total in
    let next = ref None in
    let sum = ref 0 in
    Hashtbl.iter begin fun second count ->
      begin match !next with
      | None -> if count + !sum > rnd then next := Some second
      | _ -> ()
      end;
      sum := !sum + count
    end db';
    Option.default "." !next
  in
  let rec loop first =
    Printf.printf "%s " first;
    match first with
    | "." -> ()
    | _ -> loop (generate_next first)
  in
  loop first;
  Printf.printf "\n"

let () =
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  let h = Curl.init () in (* get a handle *)
  (* set options *)
  (*Curl.set_url h "https://api.github.com/repos/quantcast/qfs/commits";*)
  Curl.set_url h "https://api.github.com/repos/torvalds/linux/commits";
  let b = Buffer.create 10 in
  (* store everything ot buffer *)
  Curl.set_writefunction h (fun s -> Buffer.add_string b s; String.length s);
  Curl.set_useragent h "IE";
  Curl.perform h;
  Curl.global_cleanup ();

  let data = Buffer.contents b in
  let json = Yojson.Basic.from_string data in
  let open Yojson.Basic.Util in
  let commits = json |> to_list in
  let messages = List.map (fun json -> json |> member "commit" |>  member "message" |> to_string ) commits in
  List.iter print_endline messages;
  let db = process messages in
  print_db db;
  for i = 1 to 5 do
    generate db
  done

