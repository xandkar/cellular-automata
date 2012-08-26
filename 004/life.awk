#! /usr/bin/env awk -f


function CHAR_BORDER() {return "-"}
function CHAR_ALIVE()  {return "o"}
function CHAR_DEAD()   {return " "}


function get_random_state() {
    return int(2 * rand())
}


function get_init_board(n) {
    board = "";

    for (i=1; i <= n; i++) {
        board = sprintf("%s%d", board, get_random_state())
    };

    return board
}


function get_char_of_state(state) {
    if (state == 1) {
        return CHAR_ALIVE()
    } else if (state == 0) {
        return CHAR_DEAD()
    }
}


function do_print_border(x) {
    for (i=1; i <= x; i++) {
        printf CHAR_BORDER()
    };
    print
}


function do_print_board(x, n, board) {
    do_print_border(x);

    for (i=1; i <= n; i++) {
        printf "%s", get_char_of_state(substr(board, i, 1));

        if (i % x == 0) {
            printf "\n"
        }
    };

    do_print_border(x)
}


function get_new_state(state, live_neighbors) {
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


function get_new_generation(x, n, board) {
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
            neighbor_id = offsets[direction] + cell_id;

            # -----------------------------------------------------------------
            # Real neighbors within boundaries, ghosts beyond that!
            # -----------------------------------------------------------------
            if ((neighbor_id >= 1) && (neighbor_id <= n)) {
                neighbor_state = substr(board, neighbor_id, 1)
            } else {
                neighbor_state = get_random_state()
            };

            live_neighbors += neighbor_state
        }

        new_cell_state = get_new_state(cell_state, live_neighbors);
        new_board = sprintf("%s%d", new_board, new_cell_state)
    };

    return new_board
}


function life() {
    "stty size" | getline stty_size_out;
    split(stty_size_out, stty_size);

    x = stty_size[2];
    y = stty_size[1] - 3;  # Minus 1 char for each: border, border, cursor
    n = x * y;

    board = get_init_board(n);

    while (1) {
        do_print_board(x, n, board);
        board = get_new_generation(x, n, board)
    }
}


BEGIN {life()}
