life-005
========


Approach
--------
AWK's array is not a first class value - it cannot be copied and passed around.
One of approaches to work around this issue (the other I tried in life-004) is
using a single array with a new element for each generation (so that we can
refer to the previous while building new). Problem is that there are no
multidimensional arrays either...  However, facilities are provided for
simulating them by storing level dimension information in the key of the
associative array, so I ended up with board[generation, x, y] and manually
deleting a generation when it is no longer needed.
