type direction =
  N | NE | E | SE | S | SW | W | NW


let directions =
  [N; NE; E; SE; S; SW; W; NW]


let (|>) x f = f x


let char_dead  = ' '
let char_alive = 'o'


let char_of_state = function
  | 0 -> char_dead
  | 1 -> char_alive
  | _ -> assert false


let offset = function
  (* direction -> x, y *)
  | N  ->  0, -1
  | NE ->  1, -1
  | E  ->  1,  0
  | SE ->  1,  1
  | S  ->  0,  1
  | SW -> -1,  1
  | W  -> -1,  0
  | NW -> -1, -1


(* Hack to sleep less than 1 sec *)
let minisleep subsec =
  ignore (Unix.select [] [] [] subsec)


let init_board x y =
  Array.map (Array.map (fun _ -> Random.int 2)) (Array.make_matrix y x 0)


let print_board board =
  Array.iter
  (
    fun row ->
      Array.iter
      (fun state -> print_char (char_of_state state))
      row;
      print_newline ()
  )
  board


let new_state = function
  | 1, live_neighbors when live_neighbors < 2 -> 0
  | 1, live_neighbors when live_neighbors < 4 -> 1
  | 1, live_neighbors when live_neighbors > 3 -> 0
  | 0, live_neighbors when live_neighbors = 3 -> 1
  | state, _ -> state


let filter_offsides width height neighbors =
  List.filter
  (fun (x, y) -> x >= 0 && y >= 0 && x < width && y < height)
  neighbors


let neighbors x y =
  List.map
  (
    fun d ->
      let off_x, off_y = offset d in
      (x + off_x), (y + off_y)
  )
  directions


let new_generation board =
  let h = Array.length board
  and w = Array.length board.(0) in
  Array.mapi
  (
    fun y row ->
      Array.mapi
      (
        fun x state->
          let neighbors = neighbors x y |> filter_offsides w h in
          let states = List.map (fun (x, y) -> board.(y).(x)) neighbors in
          let live_neighbors = List.fold_left (+) 0 states in
          new_state (state, live_neighbors)
      )
      row
  )
  board


let rec life_loop board =
  print_board board;
  print_newline ();
  minisleep 0.1;
  life_loop (new_generation board)


let main argv =
  let x = int_of_string argv.(1)
  and y = int_of_string argv.(2)
  in

  Random.init (int_of_float (Unix.time ()));

  life_loop (init_board x y)


let () = main Sys.argv
