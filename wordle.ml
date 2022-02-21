module StrSet = Set.Make(String)

let secrets_path = "data/words"
let guesses_path = "data/guesses"
let _ = Random.self_init ()
exception Found of string

type guess_status = Correct | Misplaced | None
let string_of_status = function
    | Correct -> "Y"
    | Misplaced -> "M"
    | None -> "N"

let string_of_status_array arr =
    Array.fold_left (fun acc e -> acc ^ (string_of_status e)) "" arr

let is_win_match = Array.for_all (fun x -> x = Correct)

(* Populates the set with each line of the file
 * Checks that all words are 5 letters long, discards them if not *)
let set_from_file path =
    let set = StrSet.empty in
    let ic = open_in path in
    (* internal parsing function *)
    let rec _sff set =
        try
            let line = input_line ic in
            match String.length line with
            | 5 -> let set =
                StrSet.add (String.uppercase_ascii line) set in _sff set
            | _ -> _sff set (* TO?DO warning *)
        with End_of_file -> set
    in
    try
        _sff set
    with e ->
        close_in_noerr ic;
        raise e

(* Random access of the set using fold O(N) *)
let get_random set =
    let result = ref "" in
    let i = Random.int (StrSet.cardinal set) in
    let extract_fun = fun e index ->
        if index = i then (result := e; raise (Found e))
        else (index + 1)
    in
    try
        let _ = StrSet.fold extract_fun set 0 in ""
    with
    | Found s -> s
    | e -> raise e

(* Mark all well placed secret letters that match with guess Correct *)
let wordle_cmp_exact secret guess =
    let len = String.length secret in
    let result = Array.make len None in
    for i = 0 to len - 1 do
        if String.get secret i = String.get guess i then
            Array.set result i Correct
        else
            ()
    done;
    result

(* Mark all misplaced letters in guesses that appear in secret
 * it doesn't match letters that are already well placed and cannot double
 * match letters *)
<let wordle_cmp_match secret guess exact =
    (* compare each letter of the guess with all the letters in the secret *)
    let len = String.length secret in
    let guess_match = exact in
    let secret_match = Array.copy exact in
    for i = 0 to len - 1 do
        let hasMatched = ref false in
        for j = 0 to len - 1 do
            (* match not previously matched && only once per letter && equal *)
            if (Array.get guess_match i = None) && (Array.get secret_match j = None) &&
                (not !hasMatched) && (String.get guess i = String.get secret j)
                then (
                (* Printf.printf">M(%i,%i): %c/%c\n"
                    i j (String.get guess i) (String.get secret j); *)
                hasMatched := true;
                Array.set guess_match i Misplaced;
                Array.set secret_match j Misplaced
            )
        done
    done;
    guess_match

let wordle_cmp secret guess =
    let exact = wordle_cmp_exact secret guess in
    wordle_cmp_match secret guess exact

let () =
    (* let guesses = set_from_file guesses_path in *)
    let secrets = set_from_file secrets_path in
    let secret = get_random secrets in
    print_endline secret;
    (* let turn = ref 0 in *)
    while true do
        let guess = String.uppercase_ascii (read_line ()) in
        if String.length guess <> 5 then
            Printf.printf "Guess must be %i letters long\n" (String.length secret)
        else if not (StrSet.mem guess guesses) then
            print_endline "Guess word not recognized"
        else
            let guess_result = wordle_cmp secret guess in
            print_endline (string_of_status_array guess_result);
            if is_win_match guess_result then
                raise Exit
            else
                ()
    done