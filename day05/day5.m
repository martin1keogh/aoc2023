% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day5.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.
:- import_module bool.
:- import_module ranges.
:- import_module pair.
:- import_module multi_map.
:- import_module map.


:- type range ---> r(dest_start::int, source_start::int, length::int).


:- pred parse_range(string::in, range::out) is semidet.
parse_range(Line, Range) :- (
    /*trace [io(!IO)] (io.write(Line, !IO), io.nl(!IO)),*/

    Chunks = string.split_at_char(' ', Line),
    list.map(string.to_int, Chunks, Res),
    Res = [DestS, SourcS, R],
    Range = r(DestS, SourcS, R)
).

:- pred parse_ranges(list(string)::in, list(list(range))::out) is semidet.
parse_ranges(Lines, RangesList) :- (
    list.foldl(
        pred(Line::in, Acc::in, Out::out) is semidet :- (
            (
                if parse_range(Line, R)
                then
                    (
                        Acc = [],
                        Out = [[R]]
                        ;
                        Acc = [[] | [[] | T]],
                        Out = [[R] | T]
                        ;
                        Acc = [[] | T @ [[_ | _] | _]],
                        Out = [[R] | T]
                        ;
                        Acc = [H @ [_ | _] | T],
                        Out = [[R | H] | T]
                    )
                else
                    (
                        Acc = [],
                        Out = []
                        ;
                        Acc = [_ | _],
                        Out = [[] | Acc]
                    )
            )
        ),
        Lines,
        [],
        RangesList
    )
).


:- pred extract_seeds(string::in, list(int)::out) is det.
extract_seeds(FirstLine, Seeds) :- (
    Chunks = string.split_at_char(' ', FirstLine),
    list.filter_map(string.to_int, Chunks, Seeds)
).


:- pred compute_p1(list(list(range))::in, list(int)::in, int::out) is semidet.
compute_p1(RangesList, Seeds, Min) :- (
    list.map(
        find_mapping(RangesList),
        Seeds,
        Locations
    ),
    list.foldl(
        int.min, Locations, int.max_int, Min
    )
).


:- pred find_mapping(list(list(range))::in, int::in, int::out) is semidet.
find_mapping(RangesList, !V) :- (
    (
        RangesList = [
            HumidityToLocation,
            TemperatureToHumidity,
            LightToTemperature,
            WaterToLight,
            FertilizerToWater,
            SoilToFertilizer,
            SeedToSoil
        ],
        find_mapping_rec(SeedToSoil, !V),
        find_mapping_rec(SoilToFertilizer, !V),
        find_mapping_rec(FertilizerToWater, !V),
        find_mapping_rec(WaterToLight, !V),
        find_mapping_rec(LightToTemperature, !V),
        find_mapping_rec(TemperatureToHumidity, !V),
        find_mapping_rec(HumidityToLocation, !V)
    ;
        fail
    )
).


:- pred find_mapping_rec(list(range)::in, int::in, int::out) is det.
find_mapping_rec(Ranges, In, Out) :- (
    if find_first_match(
        pred(Range::in) is semidet :- (
            In >= Range^source_start, In < Range^source_start + Range^length
        ),
        Ranges,
        FM
    )
    then
        /*trace [io(!IO)] (io.write(FM, !IO), io.nl(!IO), io.write(In, !IO), io.nl(!IO)),*/
        Out = FM^dest_start - FM^source_start + In
    else Out = In
).


main(!IO) :- (
  io.read_named_file_as_lines("/tmp/05.txt", Result, !IO),

    (
        Result = ok(Lines),
        (
            if parse_ranges(Lines, Out),
               extract_seeds(det_head(Lines), Seeds),
               compute_p1(Out, Seeds, Res)
            then
                io.write(Res, !IO), io.nl(!IO)
            else
                io.write("though", !IO)
        )
    ;
        Result = error(Error),
        Msg = io.error_message(Error),
        io.write_string(Msg, !IO)
    )
).
