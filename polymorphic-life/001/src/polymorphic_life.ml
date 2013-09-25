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


let main () =
  let pool = Matrix.create ~rows:5 ~cols:5 ~data:() in
  Matrix.iter pool ~f:(
    fun ~row ~col ~data:() -> printf "R: %d, K: %d\n" row col
  )


let () = main ()
