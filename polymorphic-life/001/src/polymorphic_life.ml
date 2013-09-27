open Core.Std


module type MATRIX = sig
  type 'a t

  val create : rs:int -> ks:int -> data:'a -> 'a t

  val get : 'a t -> r:int -> k:int -> 'a

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val mapi : 'a t -> f:(r:int -> k:int -> data:'a -> 'b) -> 'b t

  val iter : 'a t -> f:(r:int -> k:int -> data:'a -> unit) -> unit

  val print : 'a t -> to_string:('a -> string) -> unit
end

module Matrix : MATRIX = struct
  type 'a t = 'a array array

  let create ~rs ~ks ~data =
    Array.make_matrix ~dimx:rs ~dimy:ks data

  let iter t ~f =
    Array.iteri t ~f:(
      fun r ks ->
        Array.iteri ks ~f:(
          fun k data ->
            f ~r ~k ~data
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
            f ~r ~k ~data
        )
    )

  let get t ~r ~k =
    t.(r).(k)
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


let main rs ks () =
  Random.self_init ();
  let grid = Matrix.create ~rs ~ks ~data:() |> Matrix.map ~f:Conway.create in
  Matrix.print grid ~to_string:Conway.to_string


let spec =
  let summary = "Polymorphic Cellular Automata" in
  let spec =
    let open Command.Spec in
    empty
    +> flag "-rows" (optional_with_default 5 int) ~doc:"Height"
    +> flag "-cols" (optional_with_default 5 int) ~doc:"Width"
  in
  Command.basic ~summary spec main


let () = Command.run spec
