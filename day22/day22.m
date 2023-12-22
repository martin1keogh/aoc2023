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
:- import_module set.
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

:- pred shift_all_one_down(rt::in, rt::out, set(int)::out) is det.
shift_all_one_down(RTreeIn, RTreeOut, ValuesShifted) :-
    rtree.fold(
        pred(Key@box3d(XMin, XMax, YMin, YMax, ZMin, ZMax)::in, Value::in, {RTreeAccIn, ValuesShiftedAccIn}::in, {RTreeAccOut, ValuesShiftedAccOut}::out) is det :- (
            if 
                ZMin > 1.0,
                rtree.delete(Key, Value, RTreeAccIn, Tmp),
                Shifted = box3d(XMin, XMax, YMin, YMax, ZMin - 1.0, ZMax - 1.0),
                search_intersects(Tmp, Shifted) = []
            then
                rtree.insert(Shifted, Value, Tmp, RTreeAccOut),
                set.insert(Value, ValuesShiftedAccIn, ValuesShiftedAccOut)
            else
                RTreeAccOut = RTreeAccIn,
                ValuesShiftedAccOut = ValuesShiftedAccIn
        ),
        RTreeIn,
        {RTreeIn, set.init},
        {RTreeOut, ValuesShifted}
    ).

:- pred shift_down_until_stable(rt::in, rt::out, set(int)::out) is det.
shift_down_until_stable(RTreeIn, RTreeOut, ValuesShifted) :-
    shift_all_one_down(RTreeIn, RTreeStep, ShiftedStep),
    (
        if RTreeIn = RTreeStep
        then RTreeOut = RTreeStep, ValuesShifted = ShiftedStep
        else
            shift_down_until_stable(RTreeStep, RTreeOut, ShiftedRec),
            set.union(ShiftedStep, ShiftedRec, ValuesShifted)
    ).

:- pred count_destructible(rt::in, int::out, int::out) is det.
count_destructible(RTree, CountDestructible, CountChain) :-
    rtree.fold(
        pred(K::in, V::in, {CountDestructibleIn, CountChainIn}::in, {CountDestructibleOut, CountChainOut}::out) is det :- (
            if
                rtree.delete(K, V, RTree, Tmp)
            then
                shift_down_until_stable(Tmp, TmpStablized, ValuesShifted),
                (
                    if Tmp = TmpStablized
                    then
                        CountDestructibleOut = CountDestructibleIn + 1,
                        CountChainOut = CountChainIn
                    else
                        CountDestructibleOut = CountDestructibleIn,
                        CountChainOut = CountChainIn + set.count(ValuesShifted)
                )
            else
                error("Unable to delete known K/V in RTree")
        ),
        RTree,
        {0, 0},
        {CountDestructible, CountChain}
    ).

main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day22.txt", Result, !IO),

    (
        Result = ok(Rows),
        to_rtree(Rows, RTree),
        shift_down_until_stable(RTree, RTreeStable, _),
        count_destructible(RTreeStable, P1, P2),

        io.write({P1, P2}, !IO),
        io.nl(!IO)

        ;

        Result = error(Error),
        io.write(Error, !IO)
    )
).
