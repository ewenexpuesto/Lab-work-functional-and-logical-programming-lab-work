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

(** NE RIEN MODIFIER AVANT CE POINT **)

let sx = 800 (* LARGEUR DE LA FENETRE GRAPHIQUE EN POINTS *)

let sy = 600  (* HAUTEUR DE LA FENETRE GRAPHIQUE EN POINTS *)


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
        pos = List.length (line.before @ line.after);
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


(***** NE RIEN MODIFIER À PARTIR DE CE POINT **)       

let apply_action a buf =
    match a with
    | Up        -> move_up    buf
    | Left      -> move_left  buf
    | Down      -> move_down  buf
    | Right     -> move_right buf
    | Char ch   -> insert_char ch buf
    | Newline   -> create_newline buf
    | Delete    -> do_suppr buf
    | Backspace -> do_backspace buf
;;

let wopen () =
  let args = Printf.sprintf " %dx%d" sx sy in
  let _ = Graphics.open_graph args in
  let _ = Graphics.set_window_title "test" in
  ()

let font_width,font_height = 18,18

let line_height = font_height + 4

let line_width = font_width + 4
             
let default_char = Char.chr 167 



                 
let draw_square col row color c =
  let _ =
    Graphics.moveto (col*line_width+4) (Graphics.size_y () - row * line_height +2) in
  let _ = Graphics.set_color color in
  let _ = Graphics.fill_rect (col*(line_width)) (Graphics.size_y () - row * (line_width)) (line_width) (line_height) in
  let _ = Graphics.set_color Graphics.black in
  let _ = Graphics.draw_rect (col*(line_width)) (Graphics.size_y () - row * (line_width)) (line_width) (line_height)
  in
  Graphics.draw_char c



let draw_line is_current row l =
  let print i c =
    let _ = draw_square i row Graphics.white c in
    i+1
  in
  let col = List.fold_right (fun c i -> print i c) l.before 0 in
  let _ = List.fold_left print col l.after in 
  let _ =
    if is_current
    then
    let _ = Graphics.set_color Graphics.red in
      let _ = Graphics.fill_rect (col*(line_width)-2) (Graphics.size_y () - row * (line_width)) (4) (line_height) in
      Graphics.set_color Graphics.black
    else ()
  in
  ()

let draw_buffer buf =
  let print b j l =
    let _ = Format.printf "line : %d@." j in
    let _ = draw_line b j l in
    j+1
  in
  let row = List.fold_right (fun l j -> print false j l) buf.before 1 in
  let _ = print true row buf.current in
  List.fold_left (print false) (row+1) buf.after
  
  
let rec loop  buf =
  let _ = Graphics.clear_graph () in 
  let _ = draw_buffer buf in 
  let ev = Graphics.wait_next_event [Graphics.Key_pressed] in
  let ch = ev.Graphics.key in
  if Char.code ch = 27 (* esc *) 
  then ()
  else 
    let laction = [
        Char.chr 26,Up;
        Char.chr 19,Down;
        Char.chr 17,Left;
        Char.chr 4,Right;
        Char.chr 13,Newline;
        Char.chr 127,Delete;
        Char.chr 8,Backspace
      ]
    in
    let buf1 = 
      match List.assoc_opt ch laction with
      | Some a -> apply_action a buf
      | None ->
                  if String.contains valid_chars ch
         then apply_action  (Char ch) buf
         else
           let code = Char.code ch in
           let msg = if code >= 1 && code <= 26
                     then Format.sprintf " (CTRL + %c)" (Char.chr (Char.code 'A' + code -1 ))
                     else ""
           in
           let _ = 
             Format.fprintf Format.err_formatter
               "Invalid char : ascii code %d %s@."
               code
               msg
           in 
           buf
    in
    loop buf1
  
let main () =
  let _ = wopen () in
  let _ = loop empty_buf in 
  let _ = Graphics.close_graph () in
  ()

let _ = main  ()
