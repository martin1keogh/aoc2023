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


:- pred diff_list(list(int)::in, list(int)::out) is det.
diff_list([], []).
diff_list([_], []).
diff_list([A | T@[B | _]], Out) :-
    diff_list(T, Rec),
    append([B - A], Rec, Out).


:- pred compute(list(int)::in, int::out) is det.
compute(Sequence, Result) :- (
    if remove_dups(Sequence, [0])
    then
        Result = 0
    else
        diff_list(Sequence, Rec),
        compute(Rec, RecResult),
        Result = det_last(Sequence) + RecResult
).

main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day09.txt", Result, !IO),

    (
        Result = ok(Rows),
        map(parse, Rows, Sequences),
        map(compute, Sequences, ResP1),
        sum(ResP1, R),

        io.write(R, !IO),
        io.nl(!IO)
    ;
        Result = error(Error),
        Msg = io.error_message(Error),
        io.write_string(Msg, !IO)
    )
).
