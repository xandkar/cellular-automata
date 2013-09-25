open Core.Std


module type MATRIX = sig
  type 'a t

  val create : rows:int -> cols:int -> data:'a -> 'a t

  val get : 'a t -> row:int -> col:int -> 'a

  val set : 'a t -> row:int -> col:int -> data:'a -> unit

  val map : 'a t -> f:(row:int -> col:int -> data:'a -> 'b) -> 'b t

  val iter : 'a t -> f:(row:int -> col:int -> data:'a -> unit) -> unit
end

module Matrix : MATRIX = struct
  type 'a t = 'a array array

  let create ~rows ~cols ~data =
    Array.make_matrix ~dimx:rows ~dimy:cols data

  let iter t ~f =
    Array.iteri t ~f:(
      fun row cols ->
        Array.iteri cols ~f:(
          fun col data ->
            f ~row ~col ~data
        )
    )

  let map t ~f =
    Array.mapi t ~f:(
      fun row cols ->
        Array.mapi cols ~f:(
          fun col data ->
            f ~row ~col ~data
        )
    )

  let get t ~row ~col =
    t.(row).(col)

  let set t ~row ~col ~data =
    t.(row).(col) <- data
end


module type CELL = sig
  type t

  val state : t -> int

  val react : t -> states:int list -> t
end


module Conway : CELL = struct
  type t = D | A

  let state = function
    | D -> 0
    | A -> 1

  let react t ~states =
    let live_neighbors = List.fold_left states ~init:0 ~f:(+) in
    match t with
    | A when live_neighbors < 2 -> D
    | A when live_neighbors < 4 -> A
    | A when live_neighbors > 3 -> D
    | D when live_neighbors = 3 -> A
    | t -> t
end


let main rows cols () =
  let pool = Matrix.create ~rows ~cols ~data:() in
  Matrix.iter pool ~f:(
    fun ~row ~col ~data:() -> printf "R: %d, K: %d\n" row col
  )


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
