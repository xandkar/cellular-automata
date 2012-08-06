type direction =
  N | NE | E | SE | S | SW | W | NW


let directions =
  [N; NE; E; SE; S; SW; W; NW]


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
  Array.iter (fun row -> Array.iter (print_int) row; print_newline ()) board


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


let new_generation board =
  let height = Array.length board
  and width = Array.length board.(0) in
  Array.mapi
  (
    fun i_y row ->
      Array.mapi
      (fun i_x state->
        let neighbors =
          List.map
          (fun d ->
            let off_x, off_y = offset d in
            (i_x + off_x), (i_y + off_y)
          )
          directions
        in
        let neighbors = filter_offsides width height neighbors in
        let states = List.map (fun (x, y) -> board.(y).(x)) neighbors in
        let live_neighbors = List.fold_left (+) 0 states in
        let state = new_state (state, live_neighbors) in
        state
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
