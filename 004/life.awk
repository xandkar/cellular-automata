#! /usr/bin/env awk -f


function CHAR_BORDER() {return "-"}
function CHAR_ALIVE()  {return "o"}
function CHAR_DEAD()   {return " "}


function init_cell() {
    return int(2 * rand())
}


function init_board(n) {
    board = "";

    for (i=1; i <= n; i++) {
        board = sprintf("%s%d", board, init_cell())
    };

    return board
}


function print_border(x) {
    for (i=1; i <= x; i++) {
        printf CHAR_BORDER()
    };
    print;
}


function char_of_state(state) {
    if (state == 1) {
        return CHAR_ALIVE()
    } else if (state == 0) {
        return CHAR_DEAD()
    }
}


function print_board(x, n, board) {
    print_border(x);

    for (i=1; i <= n; i++) {
        printf "%s", char_of_state(substr(board, i, 1));

        if (i % x == 0) {
            printf "\n"
        }
    };

    print_border(x);
}


function new_state(state, live_neighbors) {
    if (state == 1 && live_neighbors < 2) {
        return 0
    } else if (state == 1 && live_neighbors < 4) {
        return 1
    } else if (state == 1 && live_neighbors > 3) {
        return 0
    } else if (state == 0 && live_neighbors == 3) {
        return 1
    } else {
        return state
    }
}


function new_generation(x, n, board) {
    offsets["N" ] =  - x     ;
    offsets["NE"] =  -(x - 1);
    offsets["E" ] =        1 ;
    offsets["SE"] =    x + 1 ;
    offsets["S" ] =    x     ;
    offsets["SW"] =    x - 1 ;
    offsets["W" ] =      - 1 ;
    offsets["NW"] =  -(x + 1);

    new_board = "";

    for (cell_id=1; cell_id <= n; cell_id++) {
        cell_state = substr(board, cell_id, 1);
        live_neighbors = 0;

        for (direction in offsets) {
            neighbor = offsets[direction] + cell_id;

            # Make sure we're within limmits of the board
            if ( !(neighbor < 1) && !(neighbor > n)) {
                neighbor_state = substr(board, neighbor, 1);
                live_neighbors += neighbor_state;
            }
        }

        new_cell_state = new_state(cell_state, live_neighbors);
        new_board = sprintf("%s%d", new_board, new_cell_state);
    };

    return new_board
}


function life() {
    "stty size" | getline stty_size_out;
    split(stty_size_out, stty_size);

    x = stty_size[2];
    y = stty_size[1] - 3;  # Minus 1 char for each: border, border, cursor
    n = x * y;

    board = init_board(n);

    while (1) {
        print_board(x, n, board);
        board = new_generation(x, n, board);
    }
}


BEGIN {life()}
