"use strict";

type GridLocation = {r: number, k: number};

interface GridInterface<T> {
    get            : (location: GridLocation) => T;
    map            : (f : (location: GridLocation) => T) => Grid<T>;
    moore_neighbors: (origin : GridLocation) => Array<GridLocation>;
    print          : (toString : (T: T) => string) => void;
};

class Grid<T> implements GridInterface<T> {
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
        const cells = [];
        for (let r = 0; r < rows; r++) {
            cells[r] = [];
            for (let k = 0; k < columns; k++) {
                cells[r][k] = init({r: r, k: k})
            };
        };
        this.cells = cells
    };

    get({r, k}: GridLocation) : T {
        return this.cells[r][k]
    };

    moore_neighbors(origin : GridLocation) : Array<GridLocation> {
        const offsets =
            [ {r: -1, k: -1}, {r: -1, k: 0}, {r: -1, k: 1}
            , {r:  0, k: -1},                {r:  0, k: 1}
            , {r:  1, k: -1}, {r:  1, k: 0}, {r:  1, k: 1}
            ];
        const offset_to_location =
            (offset) => {return {r: origin.r + offset.r, k: origin.k + offset.k}};
        const locations = offsets.map(offset_to_location);
        const is_location_within_bounds =
            ({r, k}) => r >= 0 && k >= 0 && r < this.rows && k < this.columns;
        return locations.filter(is_location_within_bounds)
    };

    map(f : (location: GridLocation) => T) {
        const cells = [];
        for (let r = 0; r < this.rows; r++) {
            cells[r] = [];
            for (let k = 0; k < this.columns; k++) {
                const location = {r: r, k: k};
                cells[r][k] = f(location);
            }
        };
        const init = ({r, k}) => cells[r][k];
        const grid = new Grid({rows: this.rows, columns: this.columns, init: init});
        return grid
    };

    private print_border(): void {
        process.stdout.write("+");
        for (let k = 0; k < this.columns; k++) {
            process.stdout.write("-");
        };
        process.stdout.write("+");
        process.stdout.write("\n");
    };

    print(to_string) : void {
        this.print_border();
        for (let r = 0; r < this.rows; r++) {
            process.stdout.write("|");
            for (let k = 0; k < this.columns; k++) {
                const element = this.cells[r][k];
                const element_string = to_string(element);
                process.stdout.write(element_string);
            };
            process.stdout.write("|");
            process.stdout.write("\n");
        };
        this.print_border();
    };
};

type State = "Dead" | "Alive";

type States = Array<State>;

type Board = Grid<State>;

const state_of_integer = (i : number) : State => {
    switch (i)
    { case 0 : return "Dead"
    ; case 1 : return "Alive"
    ; default: throw("No known State for integer: " + i)
    }
};

const state_to_string = (state : State) : string => {
    switch (state)
    { case "Dead" : return " "
    ; case "Alive": return "o"
    ; default     : throw("Illegal member of State type: " + state)
    }
};

const board_new = ({rows, columns} : {rows: number, columns: number}) : Board => {
    const init = (_) => state_of_integer(Math.round(Math.random()));
    return new Grid({rows: rows, columns: columns, init: init});
};

const state_is_alive = (s : State) : boolean => {
    if (s === "Alive") {
        return true
    } else if (s === "Dead") {
        return false
    } else {
        throw("Illegal member of State type: " + s)
    }
};

const state_next = (state : State, neighbor_states : Array<State>) : State => {
    const neighbors_alive = neighbor_states.filter(state_is_alive).length;
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

const board_next = (b0 : Board) : Board => {
    const b1 = b0.map(
        (location) => {
            const neighbor_locations = b0.moore_neighbors(location);
            const neighbor_states = neighbor_locations.map((l) => b0.get(l));
            const state_0 = b0.get(location);
            const state_1 = state_next(state_0, neighbor_states);
            return state_1
        }
    );
    return b1
};

const console_clear = () : void => {
    process.stdout.write("\x1B[2J");
}

const console_reset = () : void => {
    process.stdout.write("\x1B[1;1H");
}

const board_loop = (b0 : Board) : void => {
    console_reset();
    b0.print(state_to_string);
    const b1 = board_next(b0);
    setTimeout(() => board_loop(b1), 250)
};

const main = () : void => {
    const height = parseInt(process.argv[2])
    const width  = parseInt(process.argv[3])
    const b = board_new({rows: height - 3, columns: width - 2});
    console_clear();
    board_loop(b);
};

main();
