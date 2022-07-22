type Board = Vec<Vec<u8>>;

#[derive(Debug)]  // for print formatting
struct Loc {r: i32, c: i32}

// TODO state sum type

fn ch (i : u8) -> char {
    match i {
        0 => ' ',
        1 => 'o',
        _ => panic!("impossible value: {}", i)
    }
}

fn board_init(board: &mut Board) -> () {
    for row in board {
        for cell in row {
            *cell = if rand::random() {1} else {0};
        }
    }
}

fn is_inbounds(ncols: usize, nrows: usize, l: &Loc) -> bool {
       l.r > 0
    && l.r < nrows as i32
    && l.c > 0
    && l.c < ncols as i32
}

fn board_next(curr: &Board, next: &mut Board) -> () {
    let nrows = curr.len();
    let ncols = curr[0].len();

    let offsets = [
        Loc {r: -1, c: -1}, Loc {r: -1, c: 0}, Loc {r: -1, c: 1},
        Loc {r:  0, c: -1},                    Loc {r:  0, c: 1},
        Loc {r:  1, c: -1}, Loc {r:  1, c: 0}, Loc {r:  1, c: 1},
    ];

    let mut neighbors_alive;
    let mut state0;
    let mut state1;

    for r in 0..nrows {
        for c in 0..ncols {
            neighbors_alive = 0;
            state0 = curr[r][c];
            for o in &offsets {
                let neighbor_loc = Loc {r : (r as i32) + o.r, c : (c as i32) + o.c};
                if is_inbounds(ncols, nrows, &neighbor_loc) {
                    let neighbor_state =
                        curr[neighbor_loc.r as usize][neighbor_loc.c as usize];
                    neighbors_alive += neighbor_state;
                }
            }
            if (
                (state0 == 1 && (neighbors_alive == 2 || neighbors_alive == 3))
                ||
                (state0 == 0 && neighbors_alive == 3)
                )
            {
                state1 = 1;
            } else {
                state1 = 0;
            }
            next[r][c] = state1;
        }
    }
}

fn board_print(b: &Board) -> () {
    // TODO print as single buffer string, not cell-by-cell
    print!("\x1B[1;1H");  // term reset
    for r in b {
        for c in r {
            print!("{}", ch(*c));
        }
        println!();
    }
}

fn main() {
    let ncols = 80;
    let nrows = 30;
    let mut curr = vec![vec![0; ncols]; nrows];
    let mut next = vec![vec![0; ncols]; nrows];
    let mut temp = vec![vec![0; ncols]; nrows];

    board_init(&mut curr);

    print!("\x1B[2J");  // term clear
    loop {
        board_print(& curr);
        board_next(&curr, &mut next);
        temp = curr;
        curr = next;
        next = temp;
        std::thread::sleep(std::time::Duration::from_millis(250));
    }
}
