% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day22.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module rtree.
:- import_module string.

:- type rt == rtree(box3d, int).

:- pred parse_line(string::in, box3d::out) is det.
parse_line(Line, Box) :-
    if
        [CoordMin, CoordMax] = string.split_at_char('~', Line),
        map(string.to_int, string.split_at_char(',', CoordMin), [XMin, YMin, ZMin]),
        map(string.to_int, string.split_at_char(',', CoordMax), [XMax, YMax, ZMax])
    then
        Box = box3d(float(XMin), float(XMax), float(YMin), float(YMax), float(ZMin), float(ZMax))
    else
        error("Unable to parse a line (and can't be bothered to say which one.)")
    .

:- pred to_rtree(list(string)::in, rt::out) is det.
to_rtree(Input, RTree) :-
    foldl(
        pred(Line::in, {RTreeIn, I}::in, {RTreeOut, I + 1}::out) is det :- (
            parse_line(Line, Box),
            rtree.insert(Box, I, RTreeIn, RTreeOut)
        ),
        Input,
        {rtree.init, 1},
        {RTree, _}
    ).

:- pred shift_all_one_down(rt::in, rt::out) is det.
shift_all_one_down(!RTree) :-
    rtree.fold(
        pred(Key@box3d(XMin, XMax, YMin, YMax, ZMin, ZMax)::in, Value::in, AccIn::in, AccOut::out) is det :- (
            if ZMin = 1.0  % can't shift down anyway
            then AccOut = AccIn
            else
                if
                    rtree.delete(Key, Value, AccIn, Tmp),
                    Shifted = box3d(XMin, XMax, YMin, YMax, ZMin - 1.0, ZMax - 1.0),
                    search_intersects(Tmp, Shifted) = []
                then
                    rtree.insert(Shifted, Value, Tmp, AccOut)
                else
                    AccOut = AccIn
        ),
        !.RTree,
        !RTree
    ).

:- pred shift_down_until_stable(rt::in, rt::out) is det.
shift_down_until_stable(RTreeIn, RTreeOut) :-
    shift_all_one_down(RTreeIn, RTreeStep),
    (
        if RTreeIn = RTreeStep
        then RTreeOut = RTreeStep
        else shift_down_until_stable(RTreeStep, RTreeOut)
    ).

:- pred count_destructible(rt::in, int::out) is det.
count_destructible(RTree, Count) :-
    rtree.fold(
        pred(K::in, V::in, CountIn::in, CountOut::out) is det :- (
            if
                rtree.delete(K, V, RTree, Tmp),
                shift_all_one_down(Tmp, Tmp)
            then
                CountOut = CountIn + 1
            else
                CountOut = CountIn
        ),
        RTree,
        0,
        Count
    ).

main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day22.txt", Result, !IO),

    (
        Result = ok(Rows),
        to_rtree(Rows, RTree),
        shift_down_until_stable(RTree, RTreeStable),
        count_destructible(RTreeStable, P1),

        io.write(P1, !IO),
        io.nl(!IO)

        ;

        Result = error(Error),
        io.write(Error, !IO)
    )
).
