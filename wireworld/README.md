[Wireworld]
===========


States
------
* Empty
* Electron head
* Electron tail
* Conductor


Transitions
-----------
* Empty -> Empty
* Electron head -> Electron tail
* Electron tail -> Conductor
* Conductor -> Electron head if exactly one or two of the neighbouring cells
  are electron heads, or remains Conductor otherwise.


[Wireworld]: http://en.wikipedia.org/wiki/Wireworld
