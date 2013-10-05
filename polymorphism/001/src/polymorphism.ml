open Core.Std


let (|-) g f x = f (g x)


module Terminal :
sig
  type color = [ `green
               | `red
               | `white
               ]

  val string_with_color : string -> color -> string

  val clear : unit -> unit

  val reset : unit -> unit
end =
struct
  type color = [ `green
               | `red
               | `white
               ]

  let ansi_code_clear = "\027[2J"    (* Clear screen *)
  let ansi_code_reset = "\027[1;1H"  (* Reset cursor position *)

  let string_of_color = function
    | `green -> "\027[0;32m"
    | `red   -> "\027[1;31m"
    | `white -> "\027[1;37m"

  let string_with_color s c =
    sprintf "%s%s\027[0m" (string_of_color c) s

  let clear () =
    print_string ansi_code_clear

  let reset () =
    print_string ansi_code_reset
end


module Matrix :
sig
  module Point :
  sig
    type t = {r : int; k : int}
  end

  type 'a t

  val make : rs:int -> ks:int -> 'a -> 'a t

  val get_neighbors : 'a t -> Point.t -> 'a list

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val mapi : 'a t -> f:(Point.t -> 'a -> 'b) -> 'b t

  val iter : 'a t -> f:(Point.t -> 'a -> unit) -> unit

  val print : 'a t -> to_string:('a -> string) -> unit
end =
struct
  module Point =
  struct
    type t = {r : int; k : int}

    let (+) p p' =
      { r = p.r + p'.r
      ; k = p.k + p'.k
      }
  end

  module Direction =
  struct
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

  let make ~rs ~ks x =
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
    |> List.map ~f:(Point.(+) point)
    |> List.filter ~f:(is_within_bounds t)

  let get_neighbors t point =
    List.map (neighborhood t point) ~f:(get t)
end


module PhenoType :
sig
  type t

  val make : char -> Terminal.color option -> t

  val to_string : t -> string
end =
struct
  type t = { color     : Terminal.color option
           ; character : char
           }

  let make character color =
    {color; character}

  let to_string = function
    | {color=None; character} ->
      String.of_char character
    | {color=Some c; character} ->
      Terminal.string_with_color (String.of_char character) c
end


module Cell =
struct
  module State =
  struct
    type intention = Friendly
                   | Neutral
                   | Hostile

    type t = Alive of intention
           | Dead
  end

  type t = { state : State.t
           ; pheno : PhenoType.t
           }
end


module type RULE =
sig
  val init : unit -> Cell.t

  val transition : self:Cell.State.t
                -> neighbors:Cell.State.t list
                -> Cell.t
end


module Life : RULE =
struct
  module State :
  sig
    type t = D | A

    val random : unit -> t

    val is_alive : t -> bool

    val to_cell : t -> Cell.t

    val of_cell_state : Cell.State.t -> t

    val next : t -> live_neighbors:int -> t
  end =
  struct
    type t = D | A

    let random () =
      match Random.int 2 with
      | 0 -> D
      | 1 -> A
      | _ -> assert false

    let is_alive = function
      | D -> false
      | A -> true

    let to_pheno = function
      | D -> PhenoType.make ' ' None
      | A -> PhenoType.make 'o' (Some `white)

    let of_cell_state = function
      | Cell.State.Dead                      -> D
      | Cell.State.Alive Cell.State.Friendly -> A
      | Cell.State.Alive Cell.State.Neutral  -> A
      | Cell.State.Alive Cell.State.Hostile  -> D

    let to_cell_state = function
      | D -> Cell.State.Dead
      | A -> Cell.State.Alive Cell.State.Neutral

    let to_cell t =
      { Cell.state = t |> to_cell_state
      ; Cell.pheno = t |> to_pheno
      }

    let next t ~live_neighbors =
      match t with
      | A when live_neighbors < 2 -> D
      | A when live_neighbors < 4 -> A
      | A when live_neighbors > 3 -> D
      | D when live_neighbors = 3 -> A
      | A -> A
      | D -> D
  end

  let init =
    State.random |- State.to_cell

  let count_of_live =
       List.map    ~f:State.of_cell_state
    |- List.filter ~f:State.is_alive
    |- List.length

  let transition ~self ~neighbors =
    self |> State.of_cell_state
         |> State.next ~live_neighbors:(count_of_live neighbors)
         |> State.to_cell
end


module ForestFire : RULE =
struct
  module State :
  sig
    type t = E | T | B

    val is_burning : t -> bool

    val random : unit -> t

    val to_cell : t -> Cell.t

    val of_cell_state : Cell.State.t -> t

    val next : t -> burning_neighbors:int -> t
  end =
  struct
    type t = E | T | B

    let is_burning = function
      | E -> false
      | T -> false
      | B -> true

    let random () =
      match Random.int 3 with
      | 0 -> E
      | 1 -> T
      | 2 -> B
      | _ -> assert false

    let to_pheno = function
      | E -> PhenoType.make ' ' None
      | T -> PhenoType.make 'T' (Some `green)
      | B -> PhenoType.make '#' (Some `red)

    let of_cell_state = function
      | Cell.State.Dead                      -> E
      | Cell.State.Alive Cell.State.Friendly -> T
      | Cell.State.Alive Cell.State.Neutral  -> E
      | Cell.State.Alive Cell.State.Hostile  -> B

    let to_cell_state = function
      | E -> Cell.State.Dead
      | T -> Cell.State.Alive Cell.State.Friendly
      | B -> Cell.State.Alive Cell.State.Hostile

    let to_cell t =
      { Cell.state = t |> to_cell_state
      ; Cell.pheno = t |> to_pheno
      }

    let f = 0.000001  (* Probability of spontaneous ignition *)
    let p = 0.1       (* Probability of spontaneous growth *)

    let is_probable p =
      (Random.float 1.0) <= p

    let next t ~burning_neighbors =
      match t, burning_neighbors with
      | E, _ when is_probable p         -> T
      | E, _                            -> E
      | T, 0 when is_probable f         -> B
      | T, _ when burning_neighbors > 0 -> B
      | T, _                            -> T
      | B, _                            -> E
  end

  let init =
    State.random |- State.to_cell

  let count_of_burning =
       List.map    ~f:State.of_cell_state
    |- List.filter ~f:State.is_burning
    |- List.length

  let transition ~self ~neighbors =
    self |> State.of_cell_state
         |> State.next ~burning_neighbors:(count_of_burning neighbors)
         |> State.to_cell
end


module Automaton :
sig
  type t

  val make  : rows:int
             -> columns:int
             -> interval:float
             -> rules: (module RULE) list
             -> t

  val loop : t -> unit
end =
struct
  type cell = { data : Cell.t
              ; rule : (module RULE)
              }

  type t = { grid     : cell Matrix.t
           ; interval : Time.Span.t
           ; bar      : string
           }

  let make ~rows:rs ~columns:ks ~interval ~rules =
    let n = List.length rules in
    let init () =
      let rule = List.nth_exn rules (Random.int n) in
      let module Rule = (val rule : RULE) in
      { rule
      ; data = Rule.init ()
      }
    in
    Terminal.clear ();
    { grid     = Matrix.map ~f:init (Matrix.make ~rs ~ks ())
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
              ~self:data.Cell.state
              ~neighbors:(List.map neighbors ~f:(fun c -> c.data.Cell.state))
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


let main interval () =
  Random.self_init ();
  let rows, columns = Or_error.ok_exn Linux_ext.get_terminal_size () in
  let rules =
    [ (module Life : RULE)
    ; (module ForestFire : RULE)
    ]
  in
  Automaton.loop (Automaton.make ~rows:(rows - 3) ~columns ~interval ~rules)


let spec =
  let summary = "Polymorphic Cellular Automata" in
  let spec = Command.Spec.(empty
    +> flag "-i" (optional_with_default 0.1 float)
             ~doc:" Induced interval between generations."
    )
  in
  Command.basic ~summary spec main


let () = Command.run spec
