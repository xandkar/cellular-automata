type State = "Dead" | "Alive";

type States = Array<State>;

type Board = Array<States>;

type GridLocation = {r: number, k: number};

class Grid<T> {
    private rows    : number;
    private columns : number;
    private cells   : Array<Array<T>>;

    constructor(
        {rows, columns, init} :
        { rows    : number
        , columns : number
        , init    : (location: GridLocation) => T
        }
    )
    {
        this.rows    = rows;
        this.columns = columns;
        let cells = [];
        for (let r = 0; r < rows; r++) {
            cells[r] = [];
            for (let k = 0; k < columns; k++) {
                cells[r][k] = init({r: r, k: k})
            };
        };
        this.cells = cells
    };

    moore_neighbors(origin : GridLocation) : Array<GridLocation> {
        let offsets =
            [ {r: -1, k: -1}, {r: -1, k: 0}, {r: -1, k: 1}
            , {r:  0, k: -1},                {r:  0, k: 1}
            , {r:  1, k: -1}, {r:  1, k: 0}, {r:  1, k: 1}
            ];
        let offset_to_location =
            (offset) => {return {r: origin.r + offset.r, k: origin.k + offset.k}};
        let locations = offsets.map(offset_to_location);
        let is_location_within_bounds =
            ({r, k}) => r >= 0 && k >= 0 && r < this.rows && k < this.columns;
        return locations.filter(is_location_within_bounds)
    };

    mapi(f : (location: GridLocation) => T) {
        let cells = [];
        for (let r = 0; r < this.rows; r++) {
            cells[r] = [];
            for (let k = 0; k < this.columns; k++) {
                let location = {r: r, k: k};
                let neighbors = this.moore_neighbors(location);
                cells[r][k] = f(location);
            }
        };
        let init = ({r, k}) => cells[r][k];
        let grid = new Grid({rows: this.rows, columns: this.columns, init: init});
        return grid
    };
};

let state_of_integer = (i : number) : State => {
    switch (i)
    { case 0 : return "Dead"
    ; case 1 : return "Alive"
    }
};

let state_to_string = (state) : string => {
    switch (state)
    { case "Dead" : return " "
    ; case "Alive": return "o"
    }
};

let board_new = ({rows, columns} : {rows: number, columns: number}) : Board => {
    let b = [];
    for (let r = 0; r < rows; r++) {
        b[r] = [];
        for (let k = 0; k < columns; k++) {
            let zero_or_one = Math.round(Math.random());
            b[r][k] = state_of_integer(zero_or_one);
        };
    };
    return b
};

let board_neighbors = (b : Board, origin : GridLocation) : States => {
    let rows = b.length;
    let cols = b[0].length;
    let offsets =
        [ {r: -1, k: -1}, {r: -1, k: 0}, {r: -1, k: 1}
        , {r:  0, k: -1},                {r:  0, k: 1}
        , {r:  1, k: -1}, {r:  1, k: 0}, {r:  1, k: 1}
        ];
    let offset_to_location =
        (offset) => {return {r: origin.r + offset.r, k: origin.k + offset.k}};
    let locations = offsets.map(offset_to_location);
    let is_location_within_bounds =
        ({r, k}) => {
            return r >= 0 && k >= 0 && r < rows && k < cols
        };
    let locations_within_bounds = locations.filter(is_location_within_bounds);
    return locations_within_bounds.map(({r, k}) => b[r][k]);
};

let state_is_alive = (s : State) : boolean => {
    if (s === "Alive") {return true} else if (s === "Dead") {return false}
};

let state_next = (state : State, neighbor_states : Array<State>) : State => {
    let neighbors_alive = neighbor_states.filter(state_is_alive).length;
    if (state === "Alive" && neighbors_alive < 2) {
        return "Dead"
    } else if (state === "Alive" && neighbors_alive < 4) {
        return "Alive"
    } else if (state === "Alive" && neighbors_alive > 3) {
        return "Dead"
    } else if (state === "Dead" && neighbors_alive === 3) {
        return "Alive"
    } else {
        return state
    }
}

let board_next = (b0 : Board) : Board => {
    let b1 = [];
    for (let r = 0; r < b0.length; r++) {
        b1[r] = [];
        for (let k = 0; k < b0[r].length; k++) {
            let neighbors = board_neighbors(b0, {r: r, k: k});
            let state_0 = b0[r][k];
            let state_1 = state_next(state_0, neighbors);
            b1[r][k] = state_1;
        }
    };
    return b1
};

let board_print_border = (b : Board) : void => {
    process.stdout.write("+");
    for (let k = 0; k < b[0].length; k++) {
        process.stdout.write("-");
    };
    process.stdout.write("+");
    process.stdout.write("\n");
};

let board_print = (b : Board) : void => {
    board_print_border(b);
    let rows = b.length;
    let columns = b[0].length;
    for (let r = 0; r < rows; r++) {
        process.stdout.write("|");
        for (let k = 0; k < columns; k++) {
            let state = b[r][k];
            let state_string = state_to_string(state);
            process.stdout.write(state_string);
        };
        process.stdout.write("|");
        process.stdout.write("\n");
    };
    board_print_border(b);
};

let console_clear = () : void => {
    process.stdout.write("\033[2J");
}

let console_reset = () : void => {
    process.stdout.write("\033[1;1H");
}

let board_loop = (b0 : Board) : void => {
    console_reset();
    board_print(b0);
    let b1 = board_next(b0);
    setTimeout(() => board_loop(b1), 250)
};

let main = () : void => {
    let height = parseInt(process.argv[2])
    let width  = parseInt(process.argv[3])
    let b = board_new({rows: height - 3, columns: width - 2});
    console_clear();
    board_loop(b);
};

main();
