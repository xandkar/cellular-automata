"use strict";

type GridLocation = {r: number, k: number};

interface GridInterface<A> {
    get            : (location: GridLocation) => A;
    map            : (f : (location: GridLocation) => A) => Grid<A>;
    moore_neighbors: (origin : GridLocation) => Array<GridLocation>;
    print          : (toString : (A: A) => string) => void;
};

class Grid<A> implements GridInterface<A> {
    private rows    : number;
    private columns : number;
    private cells   : Array<Array<A>>;

    constructor(
        {rows, columns, init} :
        { rows    : number
        , columns : number
        , init    : (location: GridLocation) => A
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

    get({r, k}: GridLocation) : A {
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

    map(f : (location: GridLocation) => A) {
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

namespace Terminal {
    export const clear = () : void => {
        process.stdout.write("\x1B[2J");
    };

    export const reset = () : void => {
        process.stdout.write("\x1B[1;1H");
    };
};

namespace Life {
    namespace State {
        export type T = "Dead" | "Alive";

        export const of_integer = (i : number) : T => {
            switch (i)
            { case 0 : return "Dead"
            ; case 1 : return "Alive"
            ; default: throw new RangeError("No known State for integer: " + i)
            }
        };

        export const to_string = (t : T) : string => {
            switch (t)
            { case "Dead" : return " "
            ; case "Alive": return "o"
            ; default     : throw new TypeError("Illegal member of Life.State.T: " + t)
            }
        };

        export const is_alive = (t : T) : boolean => {
            switch (t)
            { case "Dead" : return false
            ; case "Alive": return true
            ; default     : throw new TypeError("Illegal member of Life.State.T: " + t)
            }
        };

        export const next = (t : T, neighbors_alive : number) : T => {
            const is_cell_alive = is_alive(t);
            if (is_cell_alive && neighbors_alive < 2) {
                return "Dead"
            } else if (is_cell_alive && neighbors_alive < 4) {
                return "Alive"
            } else if (is_cell_alive && neighbors_alive > 3) {
                return "Dead"
            } else if (!is_cell_alive && neighbors_alive === 3) {
                return "Alive"
            } else {
                return t
            }
        };
    };

    export class Board {
        private grid: Grid<State.T>;

        constructor(
            {rows, columns} :
            { rows       : number
            , columns    : number
            }
        )
        {
            const init = (_) => State.of_integer(Math.round(Math.random()));
            this.grid = new Grid({rows: rows, columns: columns, init: init});
        };

        next() : void {
            const grid = this.grid.map(
                (location) => {
                    const neighbor_locations = this.grid.moore_neighbors(location);
                    const neighbor_states = neighbor_locations.map((l) => this.grid.get(l));
                    const state_0 = this.grid.get(location);
                    const neighbors_alive = neighbor_states.filter(State.is_alive).length;
                    const state_1 = State.next(state_0, neighbors_alive);
                    return state_1
                }
            );
            this.grid = grid
        };

        print() : void {
            this.grid.print(State.to_string)
        };
    };
};

const board_loop = (b : Life.Board) : void => {
    Terminal.reset();
    b.print();
    b.next();
    setTimeout(() => board_loop(b), 250)
};

const main = () : void => {
    const height = parseInt(process.argv[2])
    const width  = parseInt(process.argv[3])
    const b = new Life.Board({rows: height - 3, columns: width - 2});
    Terminal.clear();
    board_loop(b);
};

main();
