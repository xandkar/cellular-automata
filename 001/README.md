life-001
========


Approach
--------
The grid is represented as flat list of sequential ID's, where neighbors are
calculated using the offset relative to the desired length of the printed
X-axis. See my prototyping doodles at the bottom.

Each cell is represented as a gen_server process named after its corresponding
ID on the grid and holding the names of its neighbors in its state record.

Likewise, time is represented as a gen_server process which sends ticks to all
cells and collects state responses, then proceeds to sort and print the
resulting board - this constitutes one generation.

Upon receipt of a tick, a cell broadcasts its state to its neighbors, then
proceeds to collect counterpart broadcasts from its neighbors. Upon collection
of all broadcasts - it calculates its new state and sends it to the time
process.

Additionally, there's, a non-essential, observer process, whose job is to
simply log the statistics for each run of the simulation.


Usage
-----
* Build `make`
* Run `./bin/life`


Doodles
-------
![Doodles](https://github.com/ibnfirnas/life/raw/master/001/doodles.png)
