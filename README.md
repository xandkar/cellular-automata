Games of Life
=============


Description
-----------
I want to try several different approaches to implementing Conway's Game of
Life. It shall be recorded in this repo.

Each of the implementations (living in sequentially numbered directories) shall
(eventually) have its own README file, documenting the approach taken.


Summary
-------
* __ID:__ 001,
  __Language:__ Erlang,
  __Approach:__ Each cell is a gen_server process
* __ID:__ 002,
  __Language:__ OCaml,
  __Approach:__ Traditional matrix
* __ID:__ 003,
  __Language:__ Erlang,
  __Approach:__ Traditional matrix as nested [array()]
* __ID:__ 004,
  __Language:__ AWK,
  __Approach:__ Board as string. Ghosts beyond boundaries


[array()]: http://www.erlang.org/doc/man/array.html "Which is not actually an array, but an integer-keyed tree."
