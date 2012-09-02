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
* __ID:__ 001,
  __Language:__ Erlang,
  __Approach:__ Board as 1D list. Cells as [gen_server] processes
* __ID:__ 002,
  __Language:__ OCaml,
  __Approach:__ Board as matrix via functional arrays (sort-of...)
* __ID:__ 003,
  __Language:__ Erlang,
  __Approach:__ Board as matrix via [array()]
* __ID:__ 004,
  __Language:__ AWK,
  __Approach:__ Board as 1D string. Ghosts beyond boundaries
* __ID:__ 005,
  __Language:__ AWK,
  __Approach:__ Board as simulated 3D array


[array()]: http://www.erlang.org/doc/man/array.html "Which is not actually an array, but an integer-keyed tree."
[gen_server]: http://www.erlang.org/doc/man/gen_server.html
[Conway's Game of Life]: http://en.wikipedia.org/wiki/Conways_Game_of_Life
