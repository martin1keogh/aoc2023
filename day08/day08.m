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

:- pred compute(map(string, {string, string})::in, list(dir)::in, list(dir)::in, string::in, string::in, int::out) is semidet.
compute(Doors, [], Dirs, Target, Pos, Steps) :- compute(Doors, Dirs, Dirs, Target, Pos, Steps).
compute(Doors, [H | T], Dirs, Target, Pos, Steps) :-
    (
        if suffix(Pos, Target)
        then Steps = 0
        else
            search(Doors, Pos, {L, R}),
            (
                H = l -> compute(Doors, T, Dirs, Target, L, Steps - 1);
                H = r -> compute(Doors, T, Dirs, Target, R, Steps - 1);
                fail
            )
    ).


:- pred gcd(int::in, int::in, int::out) is det.
gcd(X, Y, GCD) :- (
    X = Y -> GCD = X;
    X > Y -> gcd(Y, X, GCD);
    gcd(X, Y - X, GCD)
).


:- pred lcm(int::in, int::in, int::out) is det.
lcm(X, Y, LCM) :-
    gcd(X, Y, GCD),
    LCM = (X * Y) / GCD.


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
            compute(Doors, Dirs, Dirs, "ZZZ", "AAA", Steps),
            filter(
                pred(F::in) is semidet :- suffix(F, "A"),
                keys(Doors),
                Entrypoints
            ),
            map(compute(Doors, Dirs, Dirs, "Z"), Entrypoints, X),
            foldl(lcm, X, 1, Y)
        then
            io.write(Steps, !IO),
            io.nl(!IO),
            io.write(Y, !IO),
            io.nl(!IO)
        else
            io.write("tough", !IO)
    )
).
