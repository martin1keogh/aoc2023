% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day10.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module array2d.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module solutions.
:- import_module string.

:- pred find_start(array2d(char)::in, {int, int}::out) is nondet.
find_start(Grid, {XStart, YStart}) :- (
    bounds(Grid, XMax, YMax),
    nondet_int_in_range(0, XMax - 1, XStart),
    nondet_int_in_range(0, YMax - 1, YStart),
    lookup(Grid, XStart, YStart, 'S')
).

:- pred adjacent(array2d(char)::in, {int, int}::in, {int, int}::out) is nondet.
adjacent(Grid, {XPos, YPos}, {X, Y}) :- (
    (X = XPos - 1; X = XPos; X = XPos + 1),
    (Y = YPos - 1; Y = YPos; Y = YPos + 1),
    abs(XPos - X) + abs(YPos - Y) = 1,
    in_bounds(Grid, X, Y)
).

:- pred connection(array2d(char)::in, {int, int}::in, {int, int}::out) is nondet.
connection(Grid, {YPos, XPos}, Next) :-
    in_bounds(Grid, YPos, XPos),
    lookup(Grid, YPos, XPos, Elem),
    (
        Elem = '|' -> (Next = {YPos + 1, XPos}; Next = {YPos - 1, XPos});
        Elem = (-) -> (Next = {YPos, XPos - 1}; Next = {YPos, XPos + 1});
        Elem = 'L' -> (Next = {YPos - 1, XPos}; Next = {YPos, XPos + 1});
        Elem = 'J' -> (Next = {YPos - 1, XPos}; Next = {YPos, XPos - 1});
        Elem = '7' -> (Next = {YPos + 1, XPos}; Next = {YPos, XPos - 1});
        Elem = 'F' -> (Next = {YPos + 1, XPos}; Next = {YPos, XPos + 1});
        fail
    )
.

:- pred cycle_to_start(array2d(char)::in, {int, int}::in, {int, int}::in, set({int, int})::in, set({int, int})::out) is semidet.
cycle_to_start(Grid, From, Pos@{YPos, XPos}, !Seen) :- (
    in_bounds(Grid, YPos, XPos),
    lookup(Grid, YPos, XPos, Elem),
    insert(Pos, !Seen),
    (
        if Elem = 'S'
        then
            true
        else
            promise_equivalent_solutions [Next] (
                connection(Grid, Pos, Next),
                Next \= From
            ),
            cycle_to_start(Grid, Pos, Next, !Seen)
    )
).

:- pred find_cycles(array2d(char)::in, set({int, int})::out) is nondet.
find_cycles(Grid, Result) :-
    find_start(Grid, Start),
    adjacent(Grid, Start, NextToStart),
    connection(Grid, NextToStart, Start),
    cycle_to_start(Grid, Start, NextToStart, set.init, Result)
.

main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day10.txt", Result, !IO),

    (
        Result = ok(Rows),
        ByChars = map(string.to_char_list, Rows),
        Grid = array2d(ByChars),
        solutions(find_cycles(Grid), Cycles),

        (
            Cycles = [Cycle] ->
            CycleLength = count(Cycle),
            io.write(CycleLength / 2, !IO)
            ;
            Cycles = [] ->
            io.write("No cycles!", !IO)
            ;
            io.write("Multiple distinct cycles!", !IO)
        )

        ;

        Result = error(Error),
        io.write(Error, !IO)
    )
).
