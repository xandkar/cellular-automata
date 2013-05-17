Games of Life
=============


Description
-----------
I want to try several different approaches to implementing [Conway's Game of
Life]. It shall be recorded in this repo.

Each of the implementations (living in sequentially numbered directories) shall
(eventually) have its own README file, documenting the approach taken.


Summary
-------

| ID  | Language | Approach |
|-----|----------|----------|
| 001 | Erlang   | Board as 1D list. Cells as [gen_server] processes |
| 002 | OCaml    | Board as matrix via functional arrays (sort-of...) |
| 003 | Erlang   | Board as matrix via [array()] |
| 004 | AWK      | Board as 1D string. Ghosts beyond boundaries |
| 005 | AWK      | Board as simulated 3D array |


[array()]: http://www.erlang.org/doc/man/array.html "Which is not actually an array, but an integer-keyed tree."
[gen_server]: http://www.erlang.org/doc/man/gen_server.html
[Conway's Game of Life]: http://en.wikipedia.org/wiki/Conways_Game_of_Life
