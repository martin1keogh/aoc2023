% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day5.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.
:- import_module ranges.


:- type offset_range ---> offr(range::ranges, offset::int).


:- pred parse_range(string::in, offset_range::out) is semidet.
parse_range(Line, Range) :- (
    Chunks = string.split_at_char(' ', Line),
    list.map(string.to_int, Chunks, [Dest, Source, R]),
    Range = offr(range(Source, Source + R - 1), Dest - Source)
).

:- pred parse_ranges(list(string)::in, list(list(offset_range))::out) is det.
parse_ranges(Lines, RangesList) :- (
    list.foldr(
        pred(Line::in, {WholeAcc, GroupAcc}::in, {WholeOut, GroupOut}::out) is det :- (
            if parse_range(Line, R)
            then
                WholeOut = WholeAcc, GroupOut = [R | GroupAcc]
            else 
            (
                % Note: GroupAcc is an 'in' value, so the first part here
                % is not an assignment (or unification) but a guard.
                GroupAcc = [], WholeOut = WholeAcc, GroupOut = []
                ;
                GroupAcc = [_ | _], WholeOut = [GroupAcc | WholeAcc], GroupOut = []
            )
        ),
        Lines,
        {[], []},
        {RangesList, _}
    )
).


:- pred extract_seeds(string::in, list(int)::out) is det.
extract_seeds(FirstLine, Seeds) :- (
    Chunks = string.split_at_char(' ', FirstLine),
    list.filter_map(string.to_int, Chunks, Seeds)
).


% P2, group into pairs & create ranges
:- pred seeds_to_seed_ranges(list(int)::in, ranges::out) is semidet.
seeds_to_seed_ranges(Seeds, SeedRanges) :-
    list.foldl(
        pred([S, E]::in, In::in, Out::out) is semidet :- Out = union(In, range(S, S + E - 1)),
        list.chunk(Seeds, 2),
        ranges.empty,
        SeedRanges
    ).


:- pred compute(list(list(offset_range))::in, ranges::in, int::out) is semidet.
compute(RangesList, Seeds, Min) :- (
    find_mapping(RangesList, Seeds, Locations),
    least(Locations, Min)
).


:- pred find_mapping(list(list(offset_range))::in, ranges::in, ranges::out) is det.
find_mapping(RangesList, !V) :-
    list.foldl(find_mapping_rec, RangesList, !V).


:- pred find_mapping_rec(list(offset_range)::in, ranges::in, ranges::out) is det.
find_mapping_rec(OffsetRanges, In, Out) :- (
    list.foldl(
        pred(offr(Range, Offset)::in, {PrevMapped, Modifiables}::in, {Mapped, Unmapped}::out) is det :- (
            Common = intersection(Modifiables, Range),
            Shifted = shift(Common, Offset),

            Unmapped = difference(Modifiables, Range),
            Mapped = union(PrevMapped, Shifted)
        ),
        OffsetRanges,
        {ranges.empty, In},
        {MappedFinal, UnmappedFinal}
    ),
    Out = union(MappedFinal, UnmappedFinal)
).


main(!IO) :- (
    io.read_named_file_as_lines("/tmp/05.txt", Result, !IO),

    (
        Result = ok(Lines),
        (
            if parse_ranges(Lines, RangesList),
               extract_seeds(det_head(Lines), Seeds),
               % P1
               compute(RangesList, ranges.from_list(Seeds), Res1),
               % P2
               seeds_to_seed_ranges(Seeds, SeedRanges),
               compute(RangesList, SeedRanges, Res2)
            then
                io.write(Res1, !IO), io.nl(!IO),
                io.write(Res2, !IO), io.nl(!IO)
            else
                io.write("tough", !IO)
        )
    ;
        Result = error(Error),
        Msg = io.error_message(Error),
        io.write_string(Msg, !IO)
    )
).
