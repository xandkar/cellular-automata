open Core.Std


module Terminal : sig
  type color = [ `green
               | `red
               ]

  val string_with_color : string -> color -> string

  val clear : unit -> unit

  val reset : unit -> unit
end = struct
  type color = [ `green
               | `red
               ]

  let ansi_code_clear = "\027[2J"    (* Clear screen *)
  let ansi_code_reset = "\027[1;1H"  (* Reset cursor position *)

  let string_of_color = function
    | `green -> "\027[0;32m"
    | `red   -> "\027[1;31m"

  let string_with_color s c =
    sprintf "%s%s\027[0m" (string_of_color c) s

  let clear () =
    print_string ansi_code_clear

  let reset () =
    print_string ansi_code_reset
end


module type MATRIX = sig
  module Point : sig
    type t = {r : int; k : int}
  end

  type 'a t

  val create : rs:int -> ks:int -> 'a -> 'a t

  val get_neighbors : 'a t -> Point.t -> 'a list

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val mapi : 'a t -> f:(Point.t -> 'a -> 'b) -> 'b t

  val iter : 'a t -> f:(Point.t -> 'a -> unit) -> unit

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

  let create ~rs ~ks x =
    Array.make_matrix ~dimx:rs ~dimy:ks x

  let iter t ~f =
    Array.iteri t ~f:(
      fun r ks ->
        Array.iteri ks ~f:(
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
      fun r ks ->
        Array.mapi ks ~f:(
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

  let neighborhood t point =
    List.map Direction.all ~f:Direction.to_offset
    |> List.map ~f:(fun offset_point -> Point.(point + offset_point))
    |> List.filter ~f:(is_within_bounds t)

  let get_neighbors t point =
    List.map (neighborhood t point) ~f:(get t)
end


module Msg = struct
  type t = string
end


module State = struct
  type t = string
end


module PhenoType : sig
  type t

  val create : char -> Terminal.color option -> t

  val to_string : t -> string
end = struct
  type t = { color     : Terminal.color option
           ; character : char
           }

  let create character color =
    {color; character}

  let to_string = function
    | {color=None; character} ->
      String.of_char character
    | {color=Some c; character} ->
      Terminal.string_with_color (String.of_char character) c
end


module Cell = struct
  type t = { msg   : Msg.t
           ; pheno : PhenoType.t
           ; state : State.t
           }
end


module type RULE = sig
  val create : unit -> Cell.t

  val transition : state:State.t -> inputs:Msg.t list -> Cell.t
end


module Conway : RULE = struct
  type state = D | A

  let state_of_string : (string -> state) = function
    | "D" -> D
    | "A" -> A
    | _   -> assert false

  let state_of_int : (int -> state) = function
    | 0 -> D
    | 1 -> A
    | _ -> assert false

  let int_of_state : (state -> int) = function
    | D -> 0
    | A -> 1

  let string_of_state : (state -> string) = function
    | D -> "D"
    | A -> "A"

  let msg_of_state : (state -> Msg.t) =
    string_of_state

  let pheno_of_state : (state -> PhenoType.t) = function
    | D -> PhenoType.create ' ' None
    | A -> PhenoType.create 'o' None

  let int_of_msg msg =
    msg |> state_of_string |> int_of_state

  let next state ~live_neighbors =
    match state with
    | A when live_neighbors < 2 -> D
    | A when live_neighbors < 4 -> A
    | A when live_neighbors > 3 -> D
    | D when live_neighbors = 3 -> A
    | A -> A
    | D -> D

  let cell_of_state s =
    { Cell.msg   = s |> msg_of_state
    ; Cell.pheno = s |> pheno_of_state
    ; Cell.state = s |> string_of_state
    }

  let create () =
    Random.int 2 |> state_of_int |> cell_of_state

  let live_neighbors inputs =
    inputs |> List.map ~f:int_of_msg |> List.fold_left ~init:0 ~f:(+)

  let transition ~state ~inputs =
    state
    |> state_of_string
    |> next ~live_neighbors:(live_neighbors inputs)
    |> cell_of_state
end


module Automaton : sig
  type t

  val create  : rows:int
             -> columns:int
             -> interval:float
             -> rules: (module RULE) list
             -> t

  val loop : t -> unit
end = struct
  type cell = { data : Cell.t
              ; rule : (module RULE)
              }

  type t = { grid     : cell Matrix.t
           ; interval : Time.Span.t
           ; bar      : string
           }

  let create ~rows:rs ~columns:ks ~interval ~rules =
    let n = List.length rules in
    let i = Random.int n in
    let init () =
      let rule = List.nth_exn rules i in
      let module Rule = (val rule : RULE) in
      { rule
      ; data = Rule.create ()
      }
    in
    Terminal.clear ();
    { grid     = Matrix.map ~f:init (Matrix.create ~rs ~ks ())
    ; interval = Time.Span.of_float interval
    ; bar      = String.make ks '-'
    }

  let cell_to_string cell =
    PhenoType.to_string cell.data.Cell.pheno

  let print t =
    Terminal.reset ();
    print_endline t.bar;
    Matrix.print t.grid ~to_string:cell_to_string;
    print_endline t.bar

  let next t =
    let grid =
      Matrix.mapi t.grid ~f:(
        fun point {rule; data} ->
          let module Rule = (val rule : RULE) in
          let neighbors = Matrix.get_neighbors t.grid point in
          let data =
            Rule.transition
              ~state:data.Cell.state
              ~inputs:(List.map neighbors ~f:(fun cell -> cell.data.Cell.msg))
          in
          {rule; data}
      )
    in
    {t with grid}

  let rec loop t =
    print t;
    Time.pause t.interval;
    loop (next t)
end


let main () =
  Random.self_init ();
  let rows, columns = Or_error.ok_exn Linux_ext.get_terminal_size () in
  let interval = 0.1 in
  let rules =
    [ (module Conway : RULE)
    ]
  in
  Automaton.loop (Automaton.create ~rows:(rows - 3) ~columns ~interval ~rules)


let spec =
  let summary = "Polymorphic Cellular Automata" in
  let spec = Command.Spec.empty in
  Command.basic ~summary spec main


let () = Command.run spec
