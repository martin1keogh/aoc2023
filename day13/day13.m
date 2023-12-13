% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day13.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module bool.
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

% nondet instead of multi because we refuse to return an unmodified list (eg [] -> [])
:- pred modify_list(list(list(char))::in, list(list(char))::out) is nondet.
modify_list([[] | T], [[] | Out]) :- modify_list(T, Out).
modify_list([[H | TT] | T], Out) :- (
    (
        (H = '#' -> NH = (.); NH = '#'),
        Out = [[NH | TT] | T]
    ;
        modify_list([TT], [TTOut]),
        Out = [[H | TTOut] | T]
    ;
        modify_list(T, TOut), is_not_empty(T),
        Out = [[H | TT] | TOut]
    )
).

:- pred find_reflection(bool::in, list(list(char))::in, int::out) is nondet.
find_reflection(IsP1, Grid, Res) :-
    R = range(0, length(Grid)),
    nondet_member(I, R),
    split_list(I, Grid, L1, L2),
    is_not_empty(L1), is_not_empty(L2),
    reverse(L1, RL1),
    (IsP1 = yes -> MRL1 = RL1; modify_list(RL1, MRL1)),
    (append(MRL1, _, L2) ; append(L2, _, MRL1)),
    Res = I.

:- pred solve_for_grid(bool::in, list(list(char))::in, int::out) is nondet.
solve_for_grid(IsP1, Grid, Res) :-
    (
        find_reflection(IsP1, Grid, R), Mult = 100
        ;
        transpose(Grid, GridT),
        find_reflection(IsP1, GridT, R), Mult = 1
    ),
    Res = R * Mult,
    (
        if IsP1 = yes
        then true
        else solve_for_grid(yes, Grid, ResP1), ResP1 \= Res
    ).

:- pred sum(list(int)::in, int::out) is det.
sum(Set, Res) :-
    foldl(pred(I::in, A::in, R::out) is det :- R = I + A, Set, 0, Res).

:- pred sanity_check(list(T)::in, list(list(int))::in) is semidet.
sanity_check(Grids, Solutions) :-
    % all solutions have found 1 value per Grid
    all_true(same_length(Grids), Solutions),
    % the solution is unique
    length(Solutions, 1).

main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day13.txt", Result, !IO),

    (
        Result = ok(Rows),
        split_into_blocks(Rows, Blocks),
        map(to_grid, Blocks, Grids),
        solutions(map(solve_for_grid(yes), Grids), SolutionP1),
        (
            if not sanity_check(Grids, SolutionP1)
            then io.write({"Unexpected solution pattern in P1", SolutionP1}, !IO)
            else true
        ),
        condense(SolutionP1, FlattenedP1),
        sum(FlattenedP1, P1),
        io.write(P1, !IO),
        io.nl(!IO),
        solutions(map(solve_for_grid(no), Grids), SolutionP2),
        (
            if not sanity_check(Grids, SolutionP2)
            then io.write({"Unexpected solution pattern in P2", SolutionP2}, !IO)
            else true
        ),
        condense(SolutionP2, FlattenedP2),
        sum(FlattenedP2, P2),
        io.write(P2, !IO),
        io.nl(!IO)

        ;

        Result = error(Error),
        io.write(Error, !IO)
    )
).
