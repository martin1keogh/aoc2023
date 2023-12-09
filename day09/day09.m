% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day09.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.


:- pred parse(string::in, list(int)::out) is det.
parse(In, Out) :-
    Parts = string.split_at_string(" ", In),
    filter_map(string.to_int, Parts, Out).


:- pred sum(list(int)::in, int::out) is det.
sum([], 0).
sum([H | T], Out) :- sum(T, Out - H).


:- pred unzip(list({T, T})::in, list(T)::out, list(T)::out) is det.
unzip([], [], []).
unzip([{A, B} | T], OutA, OutB) :-
    unzip(T, TA, TB),
    append([A], TA, OutA),
    append([B], TB, OutB).


:- pred diff_list(list(int)::in, list(int)::out) is det.
diff_list([], []).
diff_list([_], []).
diff_list([A | T@[B | _]], Out) :-
    diff_list(T, Rec),
    append([B - A], Rec, Out).


:- pred compute(list(int)::in, {int, int}::out) is det.
compute(Sequence, {P1, P2}) :- (
    if all_true(pred(0::in) is semidet, Sequence)
    then
        P1 = 0, P2 = 0
    else
        diff_list(Sequence, Rec),
        compute(Rec, {RecP1, RecP2}),
        P1 = det_last(Sequence) + RecP1,
        P2 = det_head(Sequence) - RecP2
).

main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day09.txt", Result, !IO),

    (
        Result = ok(Rows),
        map(parse, Rows, Sequences),
        map(compute, Sequences, Results),
        unzip(Results, ResP1, ResP2),
        sum(ResP1, P1),
        sum(ResP2, P2),

        io.write(P1, !IO),
        io.nl(!IO),
        io.write(P2, !IO),
        io.nl(!IO)
    ;
        Result = error(Error),
        Msg = io.error_message(Error),
        io.write_string(Msg, !IO)
    )
).
