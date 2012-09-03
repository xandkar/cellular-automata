open Printf


(* ------------------------------------------------------------------------- *
 * Constants
 * ------------------------------------------------------------------------- *)
let default_f = 0.01  (* Probability of spontaneous ignition *)
let default_p = 1.0   (* Probability of spontaneous growth *)

let default_interval = 0.1  (* Induced interval between generations *)

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
  { size     : int * int
  ; prob     : float * float
  ; interval : float
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

  and interval = ref default_interval
  and f = ref default_f
  and p = ref default_p
  and x = ref default_x
  and y = ref default_y in

  let speclist =
    Arg.align
    [ ("-f", Arg.Set_float f, " Probability of spontaneous ignition.")
    ; ("-p", Arg.Set_float p, " Probability of spontaneous growth.")
    ; ("-x", Arg.Set_int x, " Forest width.")
    ; ("-y", Arg.Set_int y, " Forest height.")
    ; ("-i", Arg.Set_float interval, " Induced interval between generations.")
    ]
  in

  Arg.parse speclist (fun _ -> ()) usage;

  { size     = !x, !y
  ; prob     = !f, !p
  ; interval = !interval
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


let init_cell_state (_, p) = function
  | () when is_probable p -> Tree
  | ()                    -> Empty


let init_forest (x, y) prob =
  Array.map (Array.map (init_cell_state prob)) (Array.make_matrix y x ())


let string_of_state = function
  | Empty   -> sprintf "%c" char_empty
  | Tree    -> sprintf "%s%c%s" ansi_color_tree char_tree ansi_color_off
  | Burning -> sprintf "%s%c%s" ansi_color_burning char_burning ansi_color_off


let new_state = function
  | Burning, _, _                            -> Empty
  | Tree,    0, (f, _)    when is_probable f -> Burning
  | Tree,    n_burning, _ when n_burning > 0 -> Burning
  | Empty,   _, (_, p)    when is_probable p -> Tree
  | state,   _, _                            -> state


let print_forest forest =
  term_reset ();

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


let next_generation forest (width, height) prob =
  Array.mapi
  (
    fun iy row ->
      Array.mapi
      (
        fun ix state ->
          let neighbors = List.map (fun (ox, oy) -> ox + ix, oy + iy) offsets in
          let neighbors = List.filter (is_onside width height) neighbors in
          let neighbor_states = List.map (fun (x, y) -> forest.(y).(x)) neighbors in
          let burning_states = List.filter ((==) Burning) neighbor_states in
          new_state (state, (List.length burning_states), prob)
      )
      row
  )
  forest


let rec burn forest size prob interval =
  print_forest forest;

  if interval > 0.0 then minisleep interval;

  let next_forest = next_generation forest size prob in
  burn next_forest size prob interval


let main argv =
  Random.self_init ();

  let opts = get_opts argv in
  let forest = init_forest opts.size opts.prob in

  term_clear ();

  burn forest opts.size opts.prob opts.interval


let () = main Sys.argv
