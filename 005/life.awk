#! /usr/bin/awk -f


function CHAR_BORDER() {return "-"}
function CHAR_ALIVE()  {return "o"}
function CHAR_DEAD()   {return " "}


function get_random_state() {
    return int(2 * rand())
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


function do_print_generation(board, gen_id, y, x) {
    do_print_border(x);

    for (yi=1; yi <= y; yi++) {
        for (xi=1; xi <= x; xi++) {
            printf "%s", get_char_of_state(board[gen_id, yi, xi])
        };

        print
    }

    do_print_border(x);
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


function set_generation(board, gen_id, y, x) {
    num_directions = split("N,NE,E,SE,S,SW,W,NW", directions, ",");

    offsets["N" , "x"] =  0;
    offsets["N" , "y"] = -1;

    offsets["NE", "x"] =  1;
    offsets["NE", "y"] = -1;

    offsets["E" , "x"] =  1;
    offsets["E" , "y"] =  0;

    offsets["SE", "x"] =  1;
    offsets["SE", "y"] =  1;

    offsets["S" , "x"] =  0;
    offsets["S" , "y"] =  1;

    offsets["SW", "x"] = -1;
    offsets["SW", "y"] =  1;

    offsets["W" , "x"] = -1;
    offsets["W" , "y"] =  0;

    offsets["NW", "x"] = -1;
    offsets["NW", "y"] = -1;

    prev_gen_id = gen_id - 1;

    for (yi=1; yi <= y; yi++) {
        for (xi=1; xi <= x; xi++) {
            if (gen_id == 1) {
                board[gen_id, yi, xi] = get_random_state()
            } else {
                state = board[prev_gen_id, yi, xi];
                live_neighbors = 0;

                for (dir_i=1; dir_i <= num_directions; dir_i ++) {
                    direction = directions[dir_i];

                    xn = offsets[direction, "x"] + xi;
                    yn = offsets[direction, "y"] + yi;

                    if (xn > 0 && xn <= x && yn > 0 && yn <= y) {
                        neighbor_state = board[prev_gen_id, yn, xn];
                        live_neighbors += neighbor_state;
                    }
                };

                board[gen_id, yi, xi] = get_new_state(state, live_neighbors);
            }
        }
    }
}


function set_delete_generation(board, gen_id) {
    for (cell in board) {
        if (match(cell, "^" gen_id SUBSEP)) {
            delete board[cell]
        }
    }
}


function life() {
    "stty size" | getline stty_size_out;
    split(stty_size_out, stty_size);

    x = stty_size[2];
    y = stty_size[1] - 3;  # Minus 1 row for each: border, border, cursor

    gen_id = 0;
    while (1) {
        prev_gen_id = gen_id;
        gen_id++;

        set_generation(board, gen_id, y, x);
        do_print_generation(board, gen_id, y, x);
        set_delete_generation(board, prev_gen_id);
    }
}


BEGIN {life()}
