% vim: ft=mercury ff=unix ts=4 sw=4 et
:- module day11.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module array2d.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module set.
:- import_module solutions.
:- import_module string.


:- pred no_galaxies(list(char)::in) is semidet.
no_galaxies(Row) :-
    not any_true(pred(C::in) is semidet :- C = '#', Row).

:- pred transpose_(list(list(char))::in, list(list(char))::out) is semidet.
transpose_([], []).
transpose_([[] | _], []).
transpose_(Grid @ [[_ | _] | _], GridT) :-
    RowT = map(det_head, Grid),
    Rest = map(det_tail, Grid),
    transpose_(Rest, RestT),
    append([RowT], RestT, GridT).

:- pred transpose(list(list(char))::in, list(list(char))::out) is semidet.
:- pragma memo(transpose/2).
transpose(Grid, GridT) :- transpose_(Grid, GridT).

:- pred expand_(list(list(char))::in, list(list(char))::out) is semidet.
expand_([], []).
expand_([H | Rest], GridE) :-
    expand_(Rest, RestE),
    (
    if no_galaxies(H)
    then
        append([H, H], RestE, GridE)
    else
        append([H], RestE, GridE)
    ).

:- pred expand(list(list(char))::in, list(list(char))::out) is semidet.
expand(Grid, GridE) :-
    transpose(Grid, GridT),
    expand_(GridT, GridTE),
    transpose(GridTE, GridTET),
    expand_(GridTET, GridE).

:- pred extract_stars_row(list(char)::in, int::in, set({int, int})::out).
extract_stars_row(Row, R, Out) :-
    foldl(
        pred(C::in, {SetIn, I}::in, {SetOut, J}::out) is det :- (
            if C = '#'
            then insert({I, R}, SetIn, SetOut), J = I+1
            else SetOut = SetIn, J = I+1
        ),
        Row,
        {set.init, 0},
        {Out, _}
    ).

:- pred extract_stars(list(list(char))::in, set({int, int})::out) is det.
extract_stars(Grid, Stars) :- (
    foldl(
        pred(Row::in, {SetIn, I}::in, {SetOut, J}::out) is det :- (
            extract_stars_row(Row, I, SetRow),
            J = I + 1,
            union(SetIn, SetRow, SetOut)
        ),
        Grid,
        {set.init, 0},
        {Stars, _}
    )
).

:- pred distance_between({int, int}::in, {int, int}::in, int::out) is det.
distance_between({X1, Y1}, {X2, Y2}, Res) :-
    Res = abs(X2 - X1) + abs(Y2 - Y1).

:- pred distances_between(set({int, int})::in, {int, int}::in, list(int)::out) is det.
distances_between(Stars, Star, Distances) :-
    /*trace [io(!IO)] (io.write(Stars, !IO), io.write(Star, !IO), io.nl(!IO), io.write(Distances, !IO), io.nl(!IO)),*/
    map(distance_between(Star), to_sorted_list(Stars), Distances).

:- pred sum(list(int)::in, int::out) is det.
sum(Set, Res) :-
    foldl(pred(I::in, A::in, R::out) is det :- R = I + A, Set, 0, Res).

main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day11.txt", Result, !IO),

    (
        Result = ok(Rows),
        ByChars = map(string.to_char_list, Rows),
        Grid = lists(array2d(ByChars)),
        (
            if 
                expand(Grid, GridE),
                extract_stars(GridE, Stars)
            then
                map(distances_between(Stars), Stars, Distances),
                map(sum, to_sorted_list(Distances), SumPerStar),
                sum(SumPerStar, Sum),
                io.write(Sum, !IO),
                io.nl(!IO),
                io.write(Sum/2, !IO),
                io.nl(!IO)
            else
                io.write("welp", !IO)
        )

        ;

        Result = error(Error),
        io.write(Error, !IO)
    )
).
