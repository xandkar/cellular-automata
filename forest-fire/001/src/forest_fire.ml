open Printf


(* ------------------------------------------------------------------------- *
 * Constants
 * ------------------------------------------------------------------------- *)
let f = 0.01  (* Probability of spontaneous ignition *)
let p = 1.0   (* Probability of spontaneous growth *)

let default_x = 80
let default_y = 25

let char_empty   = ' '
let char_tree    = 'T'
let char_burning = '#'

let ansi_color_tree    = "\027[0;32m"  (* Green *)
let ansi_color_burning = "\027[1;31m"  (* Red *)
let ansi_color_off     = "\027[0m"

let ansi_code_clear = "\027[2J"    (* Clear screen *)
let ansi_code_reset = "\027[1;1H"  (* Reset cursor position *)


(* ------------------------------------------------------------------------- *
 * Types
 * ------------------------------------------------------------------------- *)
type cell_state =
  | Empty | Tree | Burning


type direction =
  | N | NE | E | SE | S | SW | W | NW


type options =
  { size : int * int
  }


(* ------------------------------------------------------------------------- *
 * Utils
 * ------------------------------------------------------------------------- *)

(* Hack to sleep less than 1 sec *)
let minisleep subsec =
  ignore (Unix.select [] [] [] subsec)


let term_clear () =
  print_string ansi_code_clear


let term_reset () =
  print_string ansi_code_reset


let get_opts argv =
  let usage = ""

  and x = ref default_x
  and y = ref default_y in

  let speclist = Arg.align [
    ("-x", Arg.Set_int x, " X.");
    ("-y", Arg.Set_int y, " Y.");
  ] in

  Arg.parse speclist (fun _ -> ()) usage;

  { size = !x, !y
  }


(* ------------------------------------------------------------------------- *
 * Core
 * ------------------------------------------------------------------------- *)
let directions =
  [N; NE; E; SE; S; SW; W; NW]


let offset_of_direction = function
  (* Direction -> x, y *)
  | N  ->  0, -1
  | NE ->  1, -1
  | E  ->  1,  0
  | SE ->  1,  1
  | S  ->  0,  1
  | SW -> -1,  1
  | W  -> -1,  0
  | NW -> -1, -1


let offsets =
  List.map (offset_of_direction) directions


let is_probable = function
  | probability when (Random.float 1.0) <= probability -> true
  | _ -> false


let init_cell_state = function
  | () when is_probable p -> Tree
  | ()                    -> Empty


let init_forest (x, y) =
  Array.map (Array.map (init_cell_state)) (Array.make_matrix y x ())


let string_of_state = function
  | Empty   -> sprintf "%c" char_empty
  | Tree    -> sprintf "%s%c%s" ansi_color_tree char_tree ansi_color_off
  | Burning -> sprintf "%s%c%s" ansi_color_burning char_burning ansi_color_off


let new_state = function
  | Burning, _                                            -> Empty
  | Tree,    0                 when is_probable f         -> Burning
  | Tree,    neighbors_burning when neighbors_burning > 0 -> Burning
  | Empty,   _                 when is_probable p         -> Tree
  | state,   _                                            -> state


let print_forest forest =
  Array.iter
  (
    fun row ->
      Array.iter
      (
        fun state ->
          print_string (string_of_state state)
      )
      row;
      print_newline ()
  )
  forest


let is_onside width height (x, y) =
  x >= 0 && y >= 0 && x < width && y < height


let next_generation forest (width, height) =
  Array.mapi
  (
    fun iy row ->
      Array.mapi
      (
        fun ix state ->
          let neighbors = List.map (fun (ox, oy) -> ox + ix, oy + iy) offsets in
          let neighbors = List.filter (is_onside width height) neighbors in
          let neighbor_states = List.map (fun (x, y) -> forest.(y).(x)) neighbors in
          let burning_states = List.filter (fun s -> s == Burning) neighbor_states in
          new_state (state, (List.length burning_states))
      )
      row
  )
  forest


let rec burn forest size =
  term_reset ();
  print_forest forest;
  minisleep 0.1;
  burn (next_generation forest size) size


let main argv =
  Random.self_init ();

  let opts = get_opts argv in
  let forest = init_forest opts.size in

  term_clear ();
  burn forest opts.size


let () = main Sys.argv
