% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day21.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module array2d.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module psqueue.
:- import_module ranges.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.

:- pred to_grid(list(string)::in, array2d(char)::out) is det.
to_grid(Input, Grid) :- Grid = array2d.from_lists(list.map(string.to_char_list, Input)).

:- pred find_start(array2d(char)::in, {int, int}::out) is det.
find_start(Grid, {X, Y}) :-
    (
        promise_equivalent_solutions [X, Y] (
            bounds(Grid, XMax, YMax),
            nondet_member(X, range(0, XMax - 1)),
            nondet_member(Y, range(0, YMax - 1)),
            lookup(Grid, X, Y, 'S')
        )
        ;
        error("unable to find starting point")
    ).

:- pred step(array2d(char)::in, {int, int}::in, {int, int}::out) is nondet.
step(Grid, {X, Y}, {XOut, YOut}) :-
    (
        % Why doesn't this form work as intended?
        % (XOut = X + 1; XOut = X - 1), YOut = Y)
        % ;
        % (XOut = X, (YOut = Y + 1; YOut = Y - 1)

        XOut = X + 1, YOut = Y
        ;
        XOut = X - 1, YOut = Y
        ;
        XOut = X, YOut = Y + 1
        ;
        XOut = X, YOut = Y - 1
    ),
    in_bounds(Grid, XOut, YOut),
    not lookup(Grid, XOut, YOut, '#').

:- pred next_unseen_step(array2d(char)::in, set({int, int})::in, {int, int}::in, {int, int}::out) is semidet.
next_unseen_step(Grid, Seen, Pos, NextPos) :-
    promise_equivalent_solutions [NextPos] (
        step(Grid, Pos, NextPos),
        not member(NextPos, Seen)
    ).

:- pred reachable_cells_under(int::in, array2d(char)::in, set({int, int})::in, set({int, int})::out, psqueue(int, {int, int})::in, psqueue(int, {int, int})::out) is det.
reachable_cells_under(N, Grid, !Seen, !PSQueue) :-
    if peek(!.PSQueue, Prio, Pos), N >= Prio
    then
    (
        if next_unseen_step(Grid, !.Seen, Pos, NextPos)
        then
            insert(NextPos, !Seen),
            (
                if insert(Prio + 1, NextPos, !PSQueue)
                then true
                else adjust(min(Prio + 1), NextPos, !PSQueue)
                ;
                error("unreachable (I think)")
            ),
            reachable_cells_under(N, Grid, !Seen, !PSQueue)
        else
            det_remove(_, Pos, !PSQueue),
            reachable_cells_under(N, Grid, !Seen, !PSQueue)
    )
    else
        true.


:- pred reachable_cells_under(int::in, array2d(char)::in, {int, int}::in, set({int, int})::out) is det.
reachable_cells_under(N, Grid, Start, SeenOut) :-
    reachable_cells_under(N, Grid, set.make_singleton_set(Start), SeenOut, psqueue.singleton(0, Start), _).

main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day21.txt", Result, !IO),

    (
        Result = ok(Rows),
        to_grid(Rows, Grid),
        find_start(Grid, Start@{XS, YS}),

        % P1
        /*solutions(reachable_cells_under(64, Grid, Start, set.init), P1Steps),*/
        reachable_cells_under(64, Grid, Start, P1Steps),
        % keep only cells with an even distance
        filter(pred({X, Y}::in) is semidet :- even((XS - X) + (YS - Y)), P1Steps, EvenDistanceSteps),
        count(EvenDistanceSteps, P1),

        io.write({Grid, Start,  EvenDistanceSteps, P1}, !IO),
        io.nl(!IO)

        ;

        Result = error(Error),
        io.write(Error, !IO)
    )
).
