module Array = ArrayLabels
module List  = ListLabels

open Printf

module Terminal : sig
  val clear : unit -> unit

  val reset : unit -> unit
end = struct
  let ansi_code_clear = "\027[2J"    (* Clear screen *)
  let ansi_code_reset = "\027[1;1H"  (* Reset cursor position *)

  let clear () =
    print_string ansi_code_clear

  let reset () =
    print_string ansi_code_reset
end

module Grid : sig
  module Point : sig
    type t = {r : int; k : int}
  end

  type 'a t

  val make : rows:int -> columns:int -> 'a -> 'a t

  val get_moore_neighbors
    : 'a t -> at:Point.t
    -> beyond_bounds:
        [ `Terminate  (* As if the point is on a traditional, flat board *)
        | `Cycle      (* As if the point is on a donut/torus *)
        ]
    -> 'a list

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val mapi : 'a t -> f:(Point.t -> 'a -> 'b) -> 'b t

  val iter : 'a t -> f:(Point.t -> 'a -> unit) -> unit

  val print : 'a t -> to_string:('a -> string) -> unit
end = struct
  module Point = struct
    type t = {r : int; k : int}

    let (+) p p' =
      { r = p.r + p'.r
      ; k = p.k + p'.k
      }

    let (mod) pa pb =
      (* FIXME: Mod returns negatives instead of cycling *)
      let rc = pa.r mod pb.r in
      let kc = pa.k mod pb.k in
      printf "pa.r: %i mod pb.r: %i -> rc: %i\n" pa.r pb.r rc ;
      printf "pa.k: %i mod pb.k: %i -> kc: %i\n" pa.k pb.k kc ;
      { r = rc
      ; k = kc
      }
  end

  module Direction = struct
    type t = NW | N | NE
           | W  |     E
           | SW | S | SE

    let all = [ NW ; N ; NE
              ; W  ;     E
              ; SW ; S ; SE
              ]

    let to_offset =
      let open Point in
      function
      | NW -> {r = -1; k = -1}
      | N  -> {r = -1; k =  0}
      | NE -> {r = -1; k =  1}
      | W  -> {r =  0; k = -1}
      | E  -> {r =  0; k =  1}
      | SW -> {r =  1; k = -1}
      | S  -> {r =  1; k =  0}
      | SE -> {r =  1; k =  1}
  end

  type 'a t = 'a array array

  let make ~rows ~columns x =
    Array.make_matrix ~dimx:rows ~dimy:columns x

  let iter t ~f =
    Array.iteri t ~f:(
      fun r columns ->
        Array.iteri columns ~f:(
          fun k x ->
            f {Point.r; Point.k} x
        )
    )

  let print t ~to_string =
    Array.iter t ~f:(
      fun r ->
        Array.iter r ~f:(fun x -> printf "%s" (to_string x));
        print_newline ()
    )

  let map t ~f =
    Array.map t ~f:(Array.map ~f:(fun x -> f x))

  let mapi t ~f =
    Array.mapi t ~f:(
      fun r columns ->
        Array.mapi columns ~f:(
          fun k x ->
            f {Point.r; Point.k} x
        )
    )

  let get t {Point.r; Point.k} =
    t.(r).(k)

  let is_within_bounds t {Point.r; Point.k} =
    match t with
    | [||] -> assert false
    | t ->
      r >= 0 && r < Array.length t &&
      k >= 0 && k < Array.length t.(0)

  let moore_neighborhood t ~at:point_origin ~beyond_bounds =
    let offsets = List.map Direction.all ~f:Direction.to_offset in
    List.fold_left offsets ~init:[] ~f:(fun points point_offset ->
      let point_neighbor = Point.(point_origin + point_offset) in
      match beyond_bounds with
      | `Terminate ->
          if is_within_bounds t point_neighbor then
            point_neighbor :: points
          else
            points
      | `Cycle ->
          let rs = Array.length t in
          let ks = Array.length t.(0) in
          let point_neighbor = Point.(point_neighbor mod {r = rs; k = ks}) in
          point_neighbor :: points
    )

  let get_moore_neighbors t ~at:point ~beyond_bounds =
    List.map (moore_neighborhood t ~at:point ~beyond_bounds) ~f:(get t)
end

module Life : sig
  type t

  type surface =
    | Board
    | Donut
    (* TODO: Mobius band *)

  val init : on:surface -> rows:int -> columns:int -> t

  val next : t -> t

  val print : t -> unit  (* Specify which quadrant to print *)
end = struct
  module State : sig
    type t = D | A

    val random : unit -> t

    val is_alive : t -> bool

    val to_string : t -> string

    val next : t -> live_neighbors:int -> t
  end = struct
    type t = D | A

    let random () =
      match Random.int 2 with
      | 0 -> D
      | 1 -> A
      | _ -> assert false

    let is_alive = function
      | D -> false
      | A -> true

    let to_string = function
      | D -> " "
      | A -> "o"

    let next t ~live_neighbors =
      match t with
      | A when live_neighbors < 2 -> D
      | A when live_neighbors < 4 -> A
      | A when live_neighbors > 3 -> D
      | D when live_neighbors = 3 -> A
      | A -> A
      | D -> D
  end

  type surface =
    | Board
    | Donut
    (* TODO: Mobius band *)
    (* TODO: Cylinder *)

  type t =
    { surface    : surface
    ; generation : int
    ; grid       : State.t Grid.t
    }

  let (|-) g f x = f (g x)

  let init ~on:surface ~rows ~columns =
    { surface
    ; generation = 1
    ; grid       = (Grid.make ~rows ~columns ()) |> Grid.map ~f:State.random
    }

  let next ({surface; generation; grid = grid_0} as t) =
    let grid_1 =
      Grid.mapi grid_0 ~f:(fun point state_0 ->
        let beyond_bounds =
          match surface with
          | Board -> `Terminate
          | Donut -> `Cycle
        in
        let live_neighbors =
          (Grid.get_moore_neighbors grid_0 ~at:point ~beyond_bounds)
          |> (List.filter ~f:State.is_alive)
          |> (List.length)
        in
        State.next state_0 ~live_neighbors
      )
    in
    {t with generation = succ generation; grid = grid_1}

  let print {generation; grid} =
    printf "Generation: %i\n" generation;
    Grid.print grid ~to_string:State.to_string
end

let rec loop life ~sleep =
  Terminal.reset ();
  Life.print life;
  Unix.sleepf sleep;
  loop (Life.next life) ~sleep

let () =
  let surface = Sys.argv.(1) in
  let rows    = Sys.argv.(2) |> int_of_string in
  let columns = Sys.argv.(3) |> int_of_string in
  let sleep   = Sys.argv.(4) |> float_of_string in
  printf
    "surface: %s, rows: %i, columns: %i, sleep: %f\n"
    surface rows columns sleep;
  let surface =
    match surface with
    | "board" -> Life.Board
    | "donut" -> Life.Donut
    | _       -> assert false
  in
  Terminal.clear ();
  loop (Life.init ~on:surface ~rows ~columns) ~sleep
