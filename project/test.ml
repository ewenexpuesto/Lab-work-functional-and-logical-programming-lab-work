let valid_chars = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" 
;;

(* zipper dont les éléments sont de type 'a et l'élément courrant de type 'b *)

type ('a,'b) zipper =
  { before:  'a list (* liste des éléments précédents dans l'ordre inversé *)
    ; current: 'b
    ; after:   'a list (* liste des éléments précédents dans l'ordre *)
    ; pos:     int (* position courrante *)
    }
;;

type cursor = Cursor;;

type line   = (char, cursor) zipper;;

type buffer = (line, line) zipper;;

type action =
    | Up
    | Left
    | Down
    | Right
    | Char of char
    | Newline
    | Delete
    | Backspace
;;

let empty_line = { before = []; current = Cursor; after = []; pos = 0 };;

let empty_buf  = { before = []; current = empty_line; after = []; pos = 0 };;


(**
 * Type: ('a, 'b) zipper -> 'b
 * @requires Nothing
 * @ensures Returns the current value of the zipper
 * @raises No exceptions
 * @return The current value stored in the zipper
 *)
let get_current z = z.current
;;

(**
 * Type: ('a, 'b) zipper -> int
 * @requires Nothing
 * @ensures Returns the position of the current value in the zipper
 * @raises No exceptions
 * @return The position of the current value in the zipper
 *)
let get_pos z = z.pos
;;

(** 
 * Type: ('a -> 'b -> 'b) -> ('c -> 'b -> 'b) -> ('a, 'c) zipper -> 'b -> 'b
 * @requires f and g must be functions that take an element and an accumulator and return an updated accumulator
 * @ensures Returns the final accumulator value after applying f to all before/after elements and g to the current element
 * @raises No exceptions
 * @return Final accumulator after folding
 *)
let fold_zipper f g z acc0 =
  let acc1 = List.fold_right f z.after acc0 in
  let acc2 = g z.current acc1 in 
  List.fold_right f z.before acc2
;;

(**
 * Type: ('b -> 'b) -> ('a, 'b) zipper -> ('a, 'b) zipper
 * @requires Nothing
 * @ensures Returns a new zipper with the current value updated by applying function f
 * @raises No exceptions
 * @return A new zipper with the updated current value
 *)
let update_with f z = {before = z.before ; current = f (get_current z) ; after = z.after ; pos = get_pos z}  
;;


(**
 * Type: buffer -> buffer
 * @requires Nothing
 * @ensures Returns a buffer with the cursor moved one position to the left if possible; unchanged if already at leftmost position
 * @raises No exceptions
 * @return The updated buffer with the cursor moved to the left
 *)
let move_left buf =
  update_with (fun line ->
    match line.before with
    | [] -> line
    | h::t -> {
        before = t;
        current = Cursor;
        after = h :: line.after;
        pos = line.pos - 1
      }
  ) buf
;;

(**
 * Type: buffer -> buffer
 * @requires Nothing
 * @ensures Returns a buffer with the cursor moved one position to the left if possible; unchanged if already at leftmost position
 * @raises No exceptions
 * @return The updated buffer with the cursor moved to the left
 *)
let move_left buf =
  match buf.current.before with
  | h :: t ->
      (* Move left within the current line, same as before *)
      update_with (fun line -> {
        before = t;
        current = Cursor;
        after = h :: line.after;
        pos = line.pos - 1
      }) buf
  | [] -> (
      match buf.before with
      | [] -> buf  (* Already at the beginning of the buffer *)
      | prev_line :: rest ->
          let new_current = prev_line in
          let new_after = get_current buf in
          let new_pos = get_pos buf - 1 in
          {before = rest; current = new_current; after = new_after :: buf.after; pos = new_pos}
    )
;;

(**
 * Type: buffer -> buffer
 * @requires Nothing
 * @ensures Returns a buffer with the cursor moved one position to the right if possible; unchanged if already at rightmost position
 * @raises No exceptions
 * @return The updated buffer with the cursor moved to the right
 *)
let move_right buf =
  update_with (fun line ->
    match line.after with
    | [] -> line
    | h::t -> {
        before = h :: line.before;
        current = Cursor;
        after = t;
        pos = line.pos + 1
      }
  ) buf
;;

(**
 * Type: buffer -> buffer
 * @requires Nothing
 * @ensures Returns a buffer with the cursor moved one position to the right if possible; unchanged if already at rightmost position
 * @raises No exceptions
 * @return The updated buffer with the cursor moved to the right
 *)
let move_right buf =
  match buf.current.after with
  | h :: t ->
      (* Move right within the current line, same as move_left *)
      update_with (fun line -> {
        before = h :: line.before;
        current = Cursor;
        after = t;
        pos = line.pos + 1
      }) buf
  | [] -> (
      match buf.after with
      | [] -> buf  (* Already at the end of the buffer *)
      | next_line :: rest ->
          let new_current = next_line in
          let new_before = get_current buf in
          let new_pos = get_pos buf + 1 in
          {before = new_before :: buf.before; current = new_current; after = rest; pos = new_pos}
    )
;;

(**
 * Type: buffer -> buffer
 * @requires Nothing
 * @ensures Returns a buffer with the cursor moved one position to the right if possible; unchanged if already at rightmost position
 * @raises No exceptions
 * @return The updated buffer with the cursor moved to the right
 *)
let move_up    buf = match buf with 
  | {before = []; current = _; after = _; pos = _} -> buf (* If this is the first line and there is none before *)
  | {before = h::t; current = c; after = a; pos = p} -> {before = t; current = h; after = c::a; pos = p - 1}
;;

let rec split_at n lst =
  if n <= 0 then ([], lst)
  else match lst with
    | [] -> ([], [])
    | x :: xs ->
        let (left, right) = split_at (n - 1) xs in
        (x :: left, right)
;;

(**
 * Type: buffer -> buffer
 * @requires Nothing
 * @ensures Returns a buffer with the cursor moved up one line if possible; if already at the top line, moves cursor to beginning of line
 * @raises No exceptions
 * @return The updated buffer with the cursor moved up
 *)
let move_up buf =
  match buf.before with
  | [] -> (* Already at the top *)
      update_with (fun line -> {
        before = [];
        current = Cursor;
        after = (List.rev line.before) @ line.after;
        pos = 0;
      }) buf
  | prev_line :: rest_before ->
      let new_after = buf.current :: buf.after in
      let new_pos = buf.pos - 1 in
      let new_current_before_rev, new_current_after = split_at (get_pos buf.current) ((List.rev prev_line.before) @ prev_line.after) in
      let new_current_before = List.rev new_current_before_rev in
      {
        before = rest_before;
        current = {before = new_current_before; current = Cursor; after = new_current_after; pos = get_pos buf.current};
        after = new_after;
        pos = new_pos
      }
;;


(**
 * Type: buffer -> buffer
 * @requires Nothing
 * @ensures Returns a buffer with the cursor moved down one line if possible; if already at the bottom line, moves cursor to end of line
 * @raises No exceptions
 * @return The updated buffer with the cursor moved down
 *)
let move_down  buf = match buf with 
| {before = _; current = _; after = []; pos = _} -> buf (* If this is the last line and there is none after *)
| {before = b; current = c; after = h::t; pos = p} -> {before = c::b; current = h; after = t; pos = p + 1}
;;

(**
 * Type: buffer -> buffer
 * @requires Nothing
 * @ensures Returns a buffer with the cursor moved down one line if possible; if already at the bottom line, moves cursor to end of line
 * @raises No exceptions
 * @return The updated buffer with the cursor moved down
 *)
let move_down buf =
  match buf.after with
  | [] -> (* Already at the bottom *)
      update_with (fun line -> {
        before = List.rev ((List.rev line.before) @ line.after);
        current = Cursor;
        after = [];
        pos = List.length (line.before @ line.after);
      }) buf
  | next_line :: rest_after ->
      let new_before = buf.current :: buf.before in
      let new_pos = buf.pos + 1 in
      let new_current_before_rev, new_current_after = split_at (get_pos buf.current) ((List.rev next_line.before) @ next_line.after) in
      let new_current_before = List.rev new_current_before_rev in
      {
        before = new_before;
        current = {before = new_current_before; current = Cursor; after = new_current_after; pos = get_pos buf.current};
        after = rest_after;
        pos = new_pos
      }
;;


(**
 * Type: char -> buffer -> buffer
 * @requires Nothing
 * @ensures Returns a buffer with the character inserted at the cursor position
 * @raises No exceptions
 * @return The updated buffer with the character inserted
 *)
 let insert_char ch buf =
  let current_line = buf.current in
  (* Create a new line with the character inserted *)
  let new_line = 
    { before = ch :: current_line.before;
      current = current_line.current;
      after = current_line.after;
      pos = current_line.pos + 1 } in
  (* Update the buffer with the new line *)
  { before = buf.before;
    current = new_line;
    after = buf.after;
    pos = buf.pos }
;;

(**
 * Type: buffer -> buffer
 * @requires Nothing
 * @ensures Returns a buffer with the character after the cursor deleted if possible; if at the end of a line, merges with the next line
 * @raises No exceptions
 * @return The updated buffer with character after cursor deleted
 *)
let do_suppr       buf =
update_with (fun line ->
  match line.after with
  | [] -> line
  | h::t -> {
      before = line.before;
      current = Cursor;
      after = t;
      pos = get_pos line
    }
) buf
;;

(**
 * Type: buffer -> buffer
 * @requires Nothing
 * @ensures Returns a buffer with the character after the cursor deleted if possible; if at the end of a line, merges with the next line
 * @raises No exceptions
 * @return The updated buffer with character after cursor deleted
 *)
let do_suppr buf =
  match buf.current.after with
  | [] -> (
      match buf.after with
      | [] -> buf  (* Nothing to delete, already at end of buffer *)
      | next_line :: next_rest -> (* Permet le retour en arrière à la fin d'une ligne *)
          let merged_after = (List.rev next_line.before) @ next_line.after in (* Reverse to line to get the original content *)
          let merged_line = {
            before = buf.current.before;
            current = Cursor;
            after = merged_after;
            pos = buf.current.pos
          } in
          {
            before = buf.before;
            current = merged_line;
            after = next_rest;
            pos = buf.pos
          }
    )
  | h :: t ->
      update_with (fun line -> {
        before = line.before;
        current = Cursor;
        after = t;
        pos = line.pos
      }) buf
;;

(**
 * Type: buffer -> buffer
 * @requires Nothing
 * @ensures Returns a buffer with the character before the cursor deleted if possible; if at the beginning of a line, merges with the previous line
 * @raises No exceptions
 * @return The updated buffer with character before cursor deleted
 *)
let do_backspace   buf =
  update_with (fun line ->
    match line.before with
    | [] -> line
    | h::t -> {
        before = t;
        current = Cursor;
        after = line.after;
        pos = get_pos line - 1
      }
  ) buf
;;

(**
 * Type: buffer -> buffer
 * @requires Nothing
 * @ensures Returns a buffer with the character before the cursor deleted if possible; if at the beginning of a line, merges with the previous line
 * @raises No exceptions
 * @return The updated buffer with character before cursor deleted
 *)
let do_backspace buf =
  match buf.current.before with
  | [] -> (
      match buf.before with
      | [] -> buf  (* Nothing to delete, already at beginning of buffer *)
      | prev_line :: prev_rest -> (* Permet le retour en arrière au début d'une ligne *)
          let merged_before = List.rev ((List.rev prev_line.before) @ prev_line.after) in (* The original data reversed so that it becomes a before *)
          let new_cursor_pos = List.length merged_before in
          let merged_line = {
            before = merged_before;
            current = Cursor;
            after = buf.current.after;
            pos = new_cursor_pos
          } in
          {
            before = prev_rest;
            current = merged_line;
            after = buf.after;
            pos = buf.pos - 1
          }
    )
  | h :: t ->
      update_with (fun line -> {
        before = t;
        current = Cursor;
        after = line.after;
        pos = line.pos - 1
      }) buf
;;

(**
 * Type: buffer -> buffer
 * @requires Nothing
 * @ensures Returns a buffer with a new empty line inserted at the cursor position, splitting the current line into two
 * @raises No exceptions
 * @return The updated buffer with a new line inserted
 *)
let create_newline buf = match buf with
  | {before = b; current = c; after = a; pos = p} -> 
      let new_line = empty_line in
      {before = c :: b; current = new_line; after = a; pos = p + 1}
;;

(**
 * Type: buffer -> buffer
 * @requires Nothing
 * @ensures Returns a buffer with a new empty line inserted at the cursor position, splitting the current line into two
 * @raises No exceptions
 * @return The updated buffer with a new line inserted
 *)
let create_newline buf = match buf with
  | {before = b; current = c; after = a; pos = p} ->
      match c with
      | {before = bb; current = Cursor; after = aa; pos = _} ->
          let before_line = { before = bb; current = Cursor; after = []; pos = List.length bb } in
          let after_line = { before = []; current = Cursor; after = aa; pos = 0 } in
          { before = before_line :: b; current = after_line; after = a; pos = p + 1 } (* Pour que le saut de ligne fasse sauter la ligne, et pas seulement le curseur *)
;;

(***** DEBUT DES TESTS **)

(* Direct manual tests for text editor functions *)

(* We'll test each function by creating specific buffers and lines manually *)
(* Then comparing the actual result with expected values *)

(* Test move_left *)
let test_move_left () =
  print_endline "\n=== Testing move_left ===";
  
  (* Test case 1: Moving left in middle of line *)
  let line1 = { before = ['l'; 'l'; 'e']; current = Cursor; after = ['H']; pos = 3 } in
  let buf1 = { before = []; current = line1; after = []; pos = 0 } in
  let result1 = move_left buf1 in
  let expected_before = ['l'; 'e'] in
  let expected_after = ['l'; 'H'] in
  let success1 = 
    (result1.current.before = expected_before) &&
    (result1.current.after = expected_after) &&
    (result1.current.pos = 2) in
  Printf.printf "Test 1 (middle of line): %s\n" (if success1 then "PASSED" else "FAILED");
  
  (* Test case 2: Moving left at beginning of line *)
  let line2 = { before = []; current = Cursor; after = ['H'; 'e'; 'l'; 'l'; 'o']; pos = 0 } in
  let buf2 = { before = []; current = line2; after = []; pos = 0 } in
  let result2 = move_left buf2 in
  let success2 = buf2 = result2 in (* Should remain unchanged *)
  Printf.printf "Test 2 (beginning of line): %s\n" (if success2 then "PASSED" else "FAILED");
  
  (* Test case 3: Moving left across lines *)
  let line3_current = { before = []; current = Cursor; after = ['L'; 'i'; 'n'; 'e'; '2']; pos = 0 } in
  let line3_before = { before = ['1'; 'e'; 'n'; 'i'; 'L']; current = Cursor; after = []; pos = 5 } in
  let buf3 = { before = [line3_before]; current = line3_current; after = []; pos = 1 } in
  let result3 = move_left buf3 in
  let success3 = 
    (List.length result3.before = 0) &&
    (result3.current.before = ['1'; 'e'; 'n'; 'i'; 'L']) &&
    (result3.current.after = []) &&
    (result3.pos = 0) in
  Printf.printf "Test 3 (across lines): %s\n" (if success3 then "PASSED" else "FAILED");
;;

(* Test move_right *)
let test_move_right () =
  print_endline "\n=== Testing move_right ===";
  
  (* Test case 1: Moving right in middle of line *)
  let line1 = { before = ['e'; 'H']; current = Cursor; after = ['l'; 'l'; 'o']; pos = 2 } in
  let buf1 = { before = []; current = line1; after = []; pos = 0 } in
  let result1 = move_right buf1 in
  let expected_before = ['l'; 'e'; 'H'] in
  let expected_after = ['l'; 'o'] in
  let success1 = 
    (result1.current.before = expected_before) &&
    (result1.current.after = expected_after) &&
    (result1.current.pos = 3) in
  Printf.printf "Test 1 (middle of line): %s\n" (if success1 then "PASSED" else "FAILED");
  
  (* Test case 2: Moving right at end of line *)
  let line2 = { before = ['o'; 'l'; 'l'; 'e'; 'H']; current = Cursor; after = []; pos = 5 } in
  let buf2 = { before = []; current = line2; after = []; pos = 0 } in
  let result2 = move_right buf2 in
  let success2 = buf2 = result2 in (* Should remain unchanged *)
  Printf.printf "Test 2 (end of line): %s\n" (if success2 then "PASSED" else "FAILED");
  
  (* Test case 3: Moving right across lines *)
  let line3_before = { before = ['1'; 'e'; 'n'; 'i'; 'L']; current = Cursor; after = []; pos = 5 } in
  let line3_after = { before = []; current = Cursor; after = ['L'; 'i'; 'n'; 'e'; '2']; pos = 0 } in
  let buf3 = { before = []; current = line3_before; after = [line3_after]; pos = 0 } in
  let result3 = move_right buf3 in
  let success3 = 
    (List.length result3.after = 0) &&
    (result3.current.before = []) &&
    (result3.current.after = ['L'; 'i'; 'n'; 'e'; '2']) &&
    (result3.pos = 1) in
  Printf.printf "Test 3 (across lines): %s\n" (if success3 then "PASSED" else "FAILED");
;;

(* Test move_up *)
let test_move_up () =
  print_endline "\n=== Testing move_up ===";
  
  (* Test case 1: Moving up from second line *)
  let line1_up = { before = ['1'; 'e'; 'n'; 'i'; 'L']; current = Cursor; after = []; pos = 5 } in
  let line1_current = { before = ['2'; 'e']; current = Cursor; after = ['n'; 'i'; 'L']; pos = 2 } in
  let buf1 = { before = [line1_up]; current = line1_current; after = []; pos = 1 } in
  let result1 = move_up buf1 in
  let success1 = 
    (result1.pos = 0) &&
    (result1.current.before = ['i'; 'L']) &&
    (result1.current.after = ['n'; 'e'; '1']) in
  Printf.printf "Test 1 (from second line): %s\n" (if success1 then "PASSED" else "FAILED");
  
  (* Test case 2: Moving up from first line *)
  let line2 = { before = ['i'; 'L']; current = Cursor; after = ['n'; 'e'; '1']; pos = 2 } in
  let buf2 = { before = []; current = line2; after = []; pos = 0 } in
  let result2 = move_up buf2 in
  let success2 = 
    (result2.current.before = []) &&
    (result2.current.after = ['L'; 'i'; 'n'; 'e'; '1']) &&
    (result2.current.pos = 0) in
  Printf.printf "Test 2 (from first line): %s\n" (if success2 then "PASSED" else "FAILED");
;;

(* Test move_down *)
let test_move_down () =
  print_endline "\n=== Testing move_down ===";
  
  (* Test case 1: Moving down from first line *)
  let line1_current = { before = ['1'; 'e']; current = Cursor; after = ['n'; 'i'; 'L']; pos = 2 } in
  let line1_down = { before = ['2'; 'e']; current = Cursor; after = ['n'; 'i'; 'L']; pos = 2 } in
  let buf1 = { before = []; current = line1_current; after = [line1_down]; pos = 0 } in
  let result1 = move_down buf1 in
  let success1 = 
    (result1.pos = 1) &&
    (result1.current.before = ['2'; 'e']) &&
    (result1.current.after = ['n'; 'i'; 'L']) in
  Printf.printf "Test 1 (from first line): %s\n" (if success1 then "PASSED" else "FAILED");
  
  (* Test case 2: Moving down from last line *)
  let line2 = { before = ['2'; 'e']; current = Cursor; after = ['n'; 'i'; 'L']; pos = 2 } in
  let buf2 = { before = []; current = line2; after = []; pos = 0 } in
  let result2 = move_down buf2 in
  let success2 = 
    (result2.current.before = ['L'; 'i'; 'n'; '2'; 'e']) &&
    (result2.current.after = []) &&
    (result2.current.pos = 5) in
  Printf.printf "Test 2 (from last line): %s\n" (if success2 then "PASSED" else "FAILED");
;;

(* Test insert_char *)
let test_insert_char () =
  print_endline "\n=== Testing insert_char ===";
  
  (* Test case 1: Insert in middle of line *)
  let line1 = { before = ['e'; 'H']; current = Cursor; after = ['l'; 'l'; 'o']; pos = 2 } in
  let buf1 = { before = []; current = line1; after = []; pos = 0 } in
  let result1 = insert_char 'X' buf1 in
  let success1 = 
    (result1.current.before = ['X'; 'e'; 'H']) &&
    (result1.current.after = ['l'; 'l'; 'o']) &&
    (result1.current.pos = 3) in
  Printf.printf "Test 1 (middle of line): %s\n" (if success1 then "PASSED" else "FAILED");
  
  (* Test case 2: Insert at beginning of line *)
  let line2 = { before = []; current = Cursor; after = ['H'; 'e'; 'l'; 'l'; 'o']; pos = 0 } in
  let buf2 = { before = []; current = line2; after = []; pos = 0 } in
  let result2 = insert_char 'X' buf2 in
  let success2 = 
    (result2.current.before = ['X']) &&
    (result2.current.after = ['H'; 'e'; 'l'; 'l'; 'o']) &&
    (result2.current.pos = 1) in
  Printf.printf "Test 2 (beginning of line): %s\n" (if success2 then "PASSED" else "FAILED");
  
  (* Test case 3: Insert at end of line *)
  let line3 = { before = ['o'; 'l'; 'l'; 'e'; 'H']; current = Cursor; after = []; pos = 5 } in
  let buf3 = { before = []; current = line3; after = []; pos = 0 } in
  let result3 = insert_char 'X' buf3 in
  let success3 = 
    (result3.current.before = ['X'; 'o'; 'l'; 'l'; 'e'; 'H']) &&
    (result3.current.after = []) &&
    (result3.current.pos = 6) in
  Printf.printf "Test 3 (end of line): %s\n" (if success3 then "PASSED" else "FAILED");
;;

(* Test do_suppr *)
let test_do_suppr () =
  print_endline "\n=== Testing do_suppr ===";
  
  (* Test case 1: Delete in middle of line *)
  let line1 = { before = ['e'; 'H']; current = Cursor; after = ['l'; 'l'; 'o']; pos = 2 } in
  let buf1 = { before = []; current = line1; after = []; pos = 0 } in
  let result1 = do_suppr buf1 in
  let success1 = 
    (result1.current.before = ['e'; 'H']) &&
    (result1.current.after = ['l'; 'o']) &&
    (result1.current.pos = 2) in
  Printf.printf "Test 1 (middle of line): %s\n" (if success1 then "PASSED" else "FAILED");
  
  (* Test case 2: Delete at end of line *)
  let line2 = { before = ['o'; 'l'; 'l'; 'e'; 'H']; current = Cursor; after = []; pos = 5 } in
  let buf2 = { before = []; current = line2; after = []; pos = 0 } in
  let result2 = do_suppr buf2 in
  let success2 = buf2 = result2 in (* Should remain unchanged *)
  Printf.printf "Test 2 (end of line): %s\n" (if success2 then "PASSED" else "FAILED");
  
  (* Test case 3: Delete to join lines *)
  let line3_current = { before = ['1'; 'e'; 'n'; 'i'; 'L']; current = Cursor; after = []; pos = 5 } in
  let line3_after = { before = []; current = Cursor; after = ['L'; 'i'; 'n'; 'e'; '2']; pos = 0 } in
  let buf3 = { before = []; current = line3_current; after = [line3_after]; pos = 0 } in
  let result3 = do_suppr buf3 in
  let success3 = 
    (result3.current.before = ['1'; 'e'; 'n'; 'i'; 'L']) &&
    (result3.current.after = ['L'; 'i'; 'n'; 'e'; '2']) &&
    (result3.pos = 0) in
  Printf.printf "Test 3 (join lines): %s\n" (if success3 then "PASSED" else "FAILED");
;;

(* Test do_backspace *)
let test_do_backspace () =
  print_endline "\n=== Testing do_backspace ===";
  
  (* Test case 1: Backspace in middle of line *)
  let line1 = { before = ['l'; 'l'; 'e']; current = Cursor; after = ['o']; pos = 3 } in
  let buf1 = { before = []; current = line1; after = []; pos = 0 } in
  let result1 = do_backspace buf1 in
  let success1 = 
    (result1.current.before = ['l'; 'e']) &&
    (result1.current.after = ['o']) &&
    (result1.current.pos = 2) in
  Printf.printf "Test 1 (middle of line): %s\n" (if success1 then "PASSED" else "FAILED");
  
  (* Test case 2: Backspace at beginning of line *)
  let line2 = { before = []; current = Cursor; after = ['H'; 'e'; 'l'; 'l'; 'o']; pos = 0 } in
  let buf2 = { before = []; current = line2; after = []; pos = 0 } in
  let result2 = do_backspace buf2 in
  let success2 = buf2 = result2 in (* Should remain unchanged *)
  Printf.printf "Test 2 (beginning of line): %s\n" (if success2 then "PASSED" else "FAILED");
  
  (* Test case 3: Backspace to join lines *)
  let line3_before = { before = ['1'; 'e'; 'n'; 'i'; 'L']; current = Cursor; after = []; pos = 5 } in
  let line3_current = { before = []; current = Cursor; after = ['L'; 'i'; 'n'; 'e'; '2']; pos = 0 } in
  let buf3 = { before = [line3_before]; current = line3_current; after = []; pos = 1 } in
  let result3 = do_backspace buf3 in
  let success3 = 
    (result3.current.before = ['1'; 'e'; 'n'; 'i'; 'L']) &&
    (result3.current.after = ['L'; 'i'; 'n'; 'e'; '2']) &&
    (result3.pos = 0) in
  Printf.printf "Test 3 (join lines): %s\n" (if success3 then "PASSED" else "FAILED");
;;

(* Test create_newline *)
let test_create_newline () =
  print_endline "\n=== Testing create_newline ===";
  
  (* Test case 1: Create newline in middle of line *)
  let line1 = { before = ['e'; 'H']; current = Cursor; after = ['l'; 'l'; 'o']; pos = 2 } in
  let buf1 = { before = []; current = line1; after = []; pos = 0 } in
  let result1 = create_newline buf1 in
  let success1 = 
    (List.length result1.before = 1) &&
    (result1.current.before = []) &&
    (result1.current.after = ['l'; 'l'; 'o']) &&
    (result1.pos = 1) in
  Printf.printf "Test 1 (middle of line): %s\n" (if success1 then "PASSED" else "FAILED");
  
  (* Test case 2: Create newline at beginning of line *)
  let line2 = { before = []; current = Cursor; after = ['H'; 'e'; 'l'; 'l'; 'o']; pos = 0 } in
  let buf2 = { before = []; current = line2; after = []; pos = 0 } in
  let result2 = create_newline buf2 in
  let success2 = 
    (List.length result2.before = 1) &&
    (result2.current.before = []) &&
    (result2.current.after = ['H'; 'e'; 'l'; 'l'; 'o']) &&
    (result2.pos = 1) in
  Printf.printf "Test 2 (beginning of line): %s\n" (if success2 then "PASSED" else "FAILED");
  
  (* Test case 3: Create newline at end of line *)
  let line3 = { before = ['o'; 'l'; 'l'; 'e'; 'H']; current = Cursor; after = []; pos = 5 } in
  let buf3 = { before = []; current = line3; after = []; pos = 0 } in
  let result3 = create_newline buf3 in
  let success3 = 
    (List.length result3.before = 1) &&
    (result3.current.before = []) &&
    (result3.current.after = []) &&
    (result3.pos = 1) in
  Printf.printf "Test 3 (end of line): %s\n" (if success3 then "PASSED" else "FAILED");
;;

(* Run all tests *)
let run_all_tests () =
  test_move_left ();
  test_move_right ();
  test_move_up ();
  test_move_down ();
  test_insert_char ();
  test_do_suppr ();
  test_do_backspace ();
  test_create_newline ();
  print_endline "\nAll tests completed.";
;;

(* Execute tests *)
run_all_tests ();;

(***** FIN DES TESTS **)