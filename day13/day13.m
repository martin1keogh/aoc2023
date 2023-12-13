% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day13.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module ranges.
:- import_module solutions.
:- import_module string.

:- pred split_into_blocks(list(string)::in, list(list(string))::out) is det.
split_into_blocks(In, Out) :-
    foldl(
        pred(Line::in, {CurrentBlock, Blocks}::in, {BlockAcc, BlocksAcc}::out) is det :- (
            if Line = ""
            then BlockAcc = [], append(Blocks, [CurrentBlock], BlocksAcc)
            else append(CurrentBlock, [Line], BlockAcc), BlocksAcc = Blocks
        ),
        In,
        {[], []},
        {LastBlock, BlocksOut}
    ),
    append(BlocksOut, [LastBlock], Out).

:- pred to_grid(list(string)::in, list(list(char))::out) is det.
to_grid(Block, Grid) :- Grid = list.map(string.to_char_list, Block).

:- pred transpose(list(list(char))::in, list(list(char))::out) is semidet.
transpose([], []).
transpose([[] | _], []).
transpose(Grid @ [[_ | _] | _], GridT) :-
    RowT = map(det_head, Grid),
    Rest = map(det_tail, Grid),
    transpose(Rest, RestT),
    append([RowT], RestT, GridT).

:- pred find_reflection(list(list(char))::in, int::out) is nondet.
find_reflection(Grid, Res) :-
    R = range(0, length(Grid)),
    nondet_member(I, R),
    split_list(I, Grid, L1, L2),
    is_not_empty(L1), is_not_empty(L2),
    reverse(L1, RL1),
    (append(RL1, _, L2) ; append(L2, _, RL1)),
    Res = I.

:- pred solve_for_grid(list(list(char))::in, int::out) is nondet.
solve_for_grid(Grid, Res) :-
    (
        find_reflection(Grid, R), Mult = 100
        ;
        transpose(Grid, GridT),
        find_reflection(GridT, R), Mult = 1
    ),
    Res = R * Mult.

:- pred sum(list(int)::in, int::out) is det.
sum(Set, Res) :-
    foldl(pred(I::in, A::in, R::out) is det :- R = I + A, Set, 0, Res).

main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day13.txt", Result, !IO),

    (
        Result = ok(Rows),
        split_into_blocks(Rows, Blocks),
        map(to_grid, Blocks, Grids),
        io.write(Grids, !IO),
        solutions(map(solve_for_grid, Grids), Solution),
        condense(Solution, Flattened),
        sum(Flattened, P1),
        io.write(P1, !IO)

        ;

        Result = error(Error),
        io.write(Error, !IO)
    )
).
