% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day14.
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

:- pred to_grid(list(string)::in, list(list(char))::out) is det.
to_grid(Block, Grid) :- Grid = list.map(string.to_char_list, Block).

:- pred transpose(list(list(char))::in, list(list(char))::out) is det.
transpose([], []).
transpose([[] | _], []).
transpose(Grid @ [[_ | _] | _], GridT) :-
    RowT = map(det_head, Grid),
    Rest = map(det_tail, Grid),
    transpose(Rest, RestT),
    append([RowT], RestT, GridT).

:- pred shift_left(list(char)::in, list(char)::out) is det.
shift_left(Chars, Out) :-
    list.foldl(
        pred(C::in, {AccIn, GroupIn}::in, {AccOut, GroupOut}::out) is det :- (
            C = '#' -> AccOut = sort(GroupIn) ++ ['#'] ++ AccIn, GroupOut = [];
            AccOut = AccIn, GroupOut = [C] ++ GroupIn
        ),
        Chars ++ ['#'],
        {[], []},
        {Acc, _}
    ),
    /*trace [io(!IO)] (io.write({Acc, Out}, !IO), io.nl(!IO)),*/
    Out = det_tail(reverse(Acc)).


:- pred score_perm(list(char)::in, int::out) is det.
score_perm(Chars, Out) :-
    L = length(Chars),
    foldl(
        pred(C::in, {S, P}::in, {O, P - 1}::out) is det :- C = 'O' -> O = S + P; O = S,
        Chars,
        {0, L},
        {Out, _}
    ).

:- pred cycle(list(list(char))::in, list(list(char))::out) is det.
cycle(Chars, Out) :-
    transpose(Chars, NorthFirst),
    map(shift_left, NorthFirst, ShiftedNorthFirst),

    transpose(ShiftedNorthFirst, WestFirst),
    map(shift_left, WestFirst, ShiftedWestFirst),

    transpose(ShiftedWestFirst, NorthFirstBeforeRev),
    SouthFirst = map(list.reverse, NorthFirstBeforeRev),
    map(shift_left, SouthFirst, ShiftedSouthFirst),

    transpose(ShiftedSouthFirst, ShiftedSouthFirstBeforeRev),
    EastFirst = map(list.reverse, ShiftedSouthFirstBeforeRev),
    map(shift_left, EastFirst, ShiftedEastFirst),

    transpose(ShiftedEastFirst, SouthFirstBeforeRev),
    SouthFirstTranspose = map(list.reverse, SouthFirstBeforeRev),
    transpose(SouthFirstTranspose, OutR),
    Out = map(list.reverse, OutR).

:- pred sum(list(int)::in, int::out) is det.
sum(Set, Res) :-
    foldl(pred(I::in, A::in, R::out) is det :- R = I + A, Set, 0, Res).

main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day14.txt", Result, !IO),

    (
        Result = ok(Rows),
        to_grid(Rows, Blocks),
        transpose(Blocks, NorthFirst),
        map(shift_left, NorthFirst, ShiftedLeft),
        map(score_perm, ShiftedLeft, Solutions),
        sum(Solutions, Solution),
        io.write(Solution, !IO),
        io.nl(!IO),

        % Too lazy to code the actual cycle detection in mercury.
        % Pipe the output of this to a file, find the cycle, compute
        % '(1_000_000_000 - line_cycle_start) % cycle_length', move down
        % that many lines.
        % Input this value as the answer, and pray.
        R = range(1, 1000),
        foldl(
            pred(_::in, CIn::in, COut::out) is det :- (
                cycle(CIn, COut),
                % output the current score we get every iteration
                transpose(COut, CT), map(score_perm, CT, SOut), sum(SOut, X),
                trace [io(!IO)] (io.write(X, !IO), io.nl(!IO))
            ),
            R,
            Blocks,
            _
        ),
        io.nl(!IO)

        ;

        Result = error(Error),
        io.write(Error, !IO)
    )
).
