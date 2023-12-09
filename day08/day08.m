% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day08.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module string.

:- type dir ---> l; r.

:- pred parse_dirs(string::in, list(dir)::out) is det.
parse_dirs(In, Out) :-
    filter_map(
        pred(C::in, D::out) is semidet :- (
            C = 'L', D = l;
            C = 'R', D = r
        ),
        to_char_list(In),
        Out
    ).

:- pred parse_row(string::in, {string, {string, string}}::out) is det.
parse_row(In, {F, {L, R}}) :- (
    between(In, 0, 3, F),
    between(In, 7, 10, L),
    between(In, 12, 15, R)
).

:- pred compute_p1(string::in, map(string, {string, string})::in, list(dir)::in, list(dir)::in, int::out) is semidet.
compute_p1(Pos, Doors, [], Dirs, Steps) :- compute_p1(Pos, Doors, Dirs, Dirs, Steps).
compute_p1(Pos, Doors, [H | T], Dirs, Steps) :-
    (
        if Pos = "ZZZ"
        then Steps = 0
        else
            search(Doors, Pos, {L, R}),
            (
                H = l -> compute_p1(L, Doors, T, Dirs, Steps - 1);
                H = r -> compute_p1(R, Doors, T, Dirs, Steps - 1);
                fail
            )
    ).


main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day08.txt", Result, !IO),

    (
        if
            Result = ok([DirsString, _ | Rows]),
            parse_dirs(DirsString, Dirs),
            map(parse_row, Rows, DoorList),
            list.foldl(
                pred({F, {L, R}}::in, AccIn::in, AccOut::out) is semidet :- map.set(F, {L, R}, AccIn, AccOut),
                DoorList, map.init, Doors
            ),
            compute_p1("AAA", Doors, Dirs, Dirs, Steps)
        then
            io.write(Steps, !IO),
            io.nl(!IO)
        else
            io.write("tough", !IO)
    )
).
