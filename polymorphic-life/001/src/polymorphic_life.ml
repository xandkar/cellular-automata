open Core.Std


module type MATRIX = sig
  module Point : sig
    type t = {r : int; k : int}
  end

  type 'a t

  val create : rs:int -> ks:int -> data:'a -> 'a t

  val get_neighbors : 'a t -> Point.t -> 'a list

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val mapi : 'a t -> f:(Point.t -> data:'a -> 'b) -> 'b t

  val iter : 'a t -> f:(Point.t -> data:'a -> unit) -> unit

  val print : 'a t -> to_string:('a -> string) -> unit
end

module Matrix : MATRIX = struct
  module Point = struct
    type t = {r : int; k : int}

    let (+) p p' =
      { r = p.r + p'.r
      ; k = p.k + p'.k
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

  let create ~rs ~ks ~data =
    Array.make_matrix ~dimx:rs ~dimy:ks data

  let iter t ~f =
    Array.iteri t ~f:(
      fun r ks ->
        Array.iteri ks ~f:(
          fun k data ->
            f {Point.r; Point.k} ~data
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
      fun r ks ->
        Array.mapi ks ~f:(
          fun k data ->
            f {Point.r; Point.k} ~data
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

  let neighborhood t point =
    List.map Direction.all ~f:Direction.to_offset
    |> List.map ~f:(fun offset_point -> Point.(point + offset_point))
    |> List.filter ~f:(is_within_bounds t)

  let get_neighbors t point =
    List.map (neighborhood t point) ~f:(get t)
end


module type CELL = sig
  type t

  val create : unit -> t

  val to_string : t -> string

  val state : t -> int

  val react : t -> states:int list -> t
end


module Conway : CELL = struct
  type t = D | A

  let of_int = function
    | 0 -> D
    | 1 -> A
    | _ -> assert false

  let to_int = function
    | D -> 0
    | A -> 1

  let to_string = function
    | D -> " "
    | A -> "o"

  let create () =
    Random.int 2 |> of_int

  let state = to_int

  let react t ~states =
    let live_neighbors = List.fold_left states ~init:0 ~f:(+) in
    match t with
    | A when live_neighbors < 2 -> D
    | A when live_neighbors < 4 -> A
    | A when live_neighbors > 3 -> D
    | D when live_neighbors = 3 -> A
    | A -> A
    | D -> D
end


let rec loop bar pause_span grid =
  print_endline bar;
  Matrix.print grid ~to_string:Conway.to_string;
  print_endline bar;
  let grid =
    Matrix.mapi grid ~f:(fun point ~data:cell ->
      let neighbors = Matrix.get_neighbors grid point in
      Conway.react cell ~states:(List.map neighbors ~f:Conway.state)
    )
  in
  Time.pause pause_span;
  loop bar pause_span grid


let main () =
  Random.self_init ();
  let rs, ks = Or_error.ok_exn Linux_ext.get_terminal_size () in
  Matrix.create ~rs:(rs - 3) ~ks ~data:()
  |> Matrix.map ~f:Conway.create
  |> loop (String.make ks '-') (Time.Span.of_float 0.1)


let spec =
  let summary = "Polymorphic Cellular Automata" in
  let spec = Command.Spec.empty in
  Command.basic ~summary spec main


let () = Command.run spec
