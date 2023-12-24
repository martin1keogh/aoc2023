% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day24.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module array2d.
:- import_module digraph.
:- import_module float.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module ranges.
:- import_module require.
:- import_module set.
/*:- import_module set_ordlist.*/
:- import_module sparse_bitset.
:- import_module solutions.
:- import_module std_util.
:- import_module string.


:- type point2d ---> point2d(
        x::float,
        y::float
).

:- type hailstone ---> hs(
        px::float,
        py::float,
        pz::float,
        vx::float,
        vy::float,
        vz::float
).

:- pred read_row(string::in, hailstone::out) is semidet.
read_row(Row, HS) :-
    [Pos, Speed] = string.split_at_string(" @ ", Row),
    StringToFloat = compose(float, compose(string.det_to_int, strip)),
    [PX, PY, PZ] = map(StringToFloat, string.split_at_string(", ", Pos)),
    [VX, VY, VZ] = map(StringToFloat, string.split_at_string(", ", Speed)),
    HS = hs(PX, PY, PZ, VX, VY, VZ).

:- func safe_div(float, float) = float is semidet.
safe_div(N, D) = R :- D = 0.0 -> fail; R = N / D.

:- pred intersect_at_2d(hailstone::in, hailstone::in, point2d::out) is semidet.
intersect_at_2d(HS1, HS2, P) :- (
    T1 = safe_div(safe_div(HS1^py - HS2^py, HS2^vy) - safe_div(HS1^px - HS2^px, HS2^vx), safe_div(HS1^vx, HS2^vx) - safe_div(HS1^vy, HS2^vy)),
    T1 >= 0.0,
    T2 = safe_div(safe_div(HS2^py - HS1^py, HS1^vy) - safe_div(HS2^px - HS1^px, HS1^vx), safe_div(HS2^vx, HS1^vx) - safe_div(HS2^vy, HS1^vy)),
    T2 >= 0.0,
    P = point2d(HS1^px + T1 * HS1^vx, HS1^py + T1 * HS1^vy)
).

:- pred intersecting_hailstones(list(hailstone)::in, {hailstone, hailstone, point2d}::out) is nondet.
intersecting_hailstones(HSs, {HS1, HS2, P}) :-
    member(HS1, HSs, Rest),
    member(HS2, Rest),
    intersect_at_2d(HS1, HS2, P).

main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day24.txt", Result, !IO),

    (
        Result = ok(Rows),
        filter_map(read_row, Rows, Hailstones),

        Min = 200000000000000.0, Max = 400000000000000.0,
        solutions(intersecting_hailstones(Hailstones), Intersections),
        filter(pred({_, _, P}::in) is semidet :- (P^x >= Min, P^x =< Max, P^y >= Min, P^y =< Max), Intersections, P1),
        length(P1, SolP1),
        io.write(SolP1, !IO),
        io.nl(!IO)

        ;

        Result = error(Error),
        io.write(Error, !IO),
        io.nl(!IO)
    )
).
