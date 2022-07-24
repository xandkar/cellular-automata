use std::io::Write;

#[derive(Clone)]
enum State {
    Dead,
    Alive,
}

type Board = Vec<Vec<State>>;

struct Loc {
    r: i32,
    c: i32,
}

const OFFSETS: [Loc; 8] = [
    Loc { r: -1, c: -1 }, // NW
    Loc { r: -1, c: 0 },  // N
    Loc { r: -1, c: 1 },  // NE
    Loc { r: 0, c: -1 },  // W
    Loc { r: 0, c: 1 },   // E
    Loc { r: 1, c: -1 },  // SW
    Loc { r: 1, c: 0 },   // S
    Loc { r: 1, c: 1 },   // SE
];

const ANSI_TERM_CLEAR: &[u8; 4] = b"\x1B[2J";
const ANSI_TERM_RESET: &[u8; 6] = b"\x1B[1;1H";

fn state_display(s: &State) -> u8 {
    match s {
        State::Dead => ' ' as u8,
        State::Alive => 'o' as u8,
    }
}

fn state_next(state: &State, neighbors_alive: &i32) -> State {
    match (state, neighbors_alive) {
        (State::Alive, 2 | 3) | (State::Dead, 3) =>
            State::Alive,
        (_, _) =>
            State::Dead,
    }
}

fn board_init(board: &mut Board) -> () {
    for row in board {
        for cell in row {
            *cell = if rand::random() { State::Alive } else { State::Dead };
        }
    }
}

fn is_inbounds(ncols: usize, nrows: usize, l: &Loc) -> bool {
    l.r > 0 && l.r < nrows as i32 && l.c > 0 && l.c < ncols as i32
}

fn board_next(curr: &Board, next: &mut Board) -> () {
    let nrows = curr.len();
    let ncols = curr[0].len();

    let mut curr_neighbors_alive;

    for r in 0..nrows {
        for c in 0..ncols {
            curr_neighbors_alive = 0;
            for o in OFFSETS {
                let neighbor = Loc {
                    r: (r as i32) + o.r,
                    c: (c as i32) + o.c,
                };
                if is_inbounds(ncols, nrows, &neighbor) {
                    match &curr[neighbor.r as usize][neighbor.c as usize] {
                        State::Alive => curr_neighbors_alive += 1,
                        State::Dead => (),
                    }
                }
            }
            next[r][c] = state_next(&curr[r][c], &curr_neighbors_alive);
        }
    }
}

fn board_display(b: &Board, buf: &mut [u8]) -> () {
    // TODO Borders.
    // TODO Stats: gen, dead, alive, death rate, clusters.
    let mut i = 0;
    for row in b {
        for cell in row {
            buf[i] = state_display(&*cell);
            i += 1;
        }
        buf[i] = '\n' as u8;
        i += 1;
    }
}

fn main() {
    const DEFAULT_NCOLS : usize = 80;
    const DEFAULT_NROWS : usize = 30;

    let (ncols, nrows) =
        match term_size::dimensions() {
            None => (DEFAULT_NCOLS, DEFAULT_NROWS),
            Some((c, r)) => (c - 5, r - 5),
        };

    let mut curr = vec![vec![State::Dead; ncols]; nrows];
    let mut next = vec![vec![State::Dead; ncols]; nrows];
    let mut buff = vec!['\0' as u8; ncols * nrows + nrows];
    let mut output = std::io::stdout();

    output.write_all(ANSI_TERM_CLEAR).unwrap();
    board_init(&mut curr);

    // TODO Controls: [p]lay/pause, [s]top, [b]ack, [f]orward.
    loop {
        board_display(&curr, &mut buff);
        output.write_all(ANSI_TERM_RESET).unwrap();
        output.write_all(&buff).unwrap();
        board_next(&curr, &mut next);
        std::mem::swap(&mut curr, &mut next);
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
}
