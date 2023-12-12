% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day12.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module solutions.
:- import_module string.

:- pred parse_row(string::in, {list(char), list(int)}::out) is semidet.
parse_row(Row, {Conditions, Groups}) :-
    [ConditionsS, GroupsS] = words_separator(char.is_whitespace, Row),
    Conditions = to_char_list(ConditionsS),
    GroupsSS = split_at_char(',', GroupsS),
    filter_map(to_int, GroupsSS, Groups).

:- pred map_conditions(list(int)::in, int::in, list(char)::in, bool::out) is nondet.
map_conditions([], 0, [], yes).
map_conditions([CurrentGroup], CurrentGroup, [], yes).
map_conditions(ToMatch, CurrentGroup, [H|T], Res) :- (
    (
        H = (.), CurrentGroup = 0,
        map_conditions(ToMatch, 0, T, Res)
        ;
        H = (.), CurrentGroup > 0, ToMatch = [CurrentGroup | Rest],
        map_conditions(Rest, 0, T, Res)
        ;
        H = '#', ToMatch = [NextToMatch | _], NextToMatch > CurrentGroup,
        map_conditions(ToMatch, CurrentGroup + 1, T, Res)
        ;
        H = '?',
        (HN = (.); HN = '#'),
        map_conditions(ToMatch, CurrentGroup, [HN]++T, Res)
    )
).

:- pred solve_for_row({list(char), list(int)}::in, int::out) is det.
solve_for_row({LI, Groups}, NumberSolutions) :-
    promise_equivalent_solutions [NumberSolutions] (
        solutions.unsorted_aggregate(
            map_conditions(Groups, 0, LI),
            pred(_::in, CountIn::in, CountOut::out) is det :- CountOut = CountIn + 1,
            0,
            NumberSolutions
        )
    )
    /*,trace [io(!IO)] (io.write({LI, Groups, NumberSolutions}, !IO), io.nl(!IO))*/
    .

:- pred sum(list(int)::in, int::out) is det.
sum([], 0).
sum([H|T], Out) :- sum(T, Out - H).

:- pred transform_chars(list(char)::in, int::in, list(char)::out) is det.
transform_chars(L, I, O) :- I = 1 -> O = L; transform_chars(L, I-1, OO), append(L ++ ['?'], OO, O).

:- pred transform_groups(list(int)::in, int::in, list(int)::out) is det.
transform_groups(L, I, O) :- I = 1 -> O = L; transform_groups(L, I-1, OO), append(L, OO, O).

:- pred transform_row(int::in, {list(char), list(int)}::in, {list(char), list(int)}::out) is det.
transform_row(Count, {LI, Groups}, {LO, GroupsOut}) :- transform_chars(LI, Count, LO), transform_groups(Groups, Count, GroupsOut).

main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day12.txt", Result, !IO),

    (
        Result = ok(Rows),
        (
            if map(parse_row, Rows, ParsedRows)
            then
                map(solve_for_row, ParsedRows, NumberSolutionsPerRowP1),
                sum(NumberSolutionsPerRowP1, ResultP1),
                io.write(ResultP1, !IO),
                io.nl(!IO),
                map(transform_row(5), ParsedRows, TransformedRows5),
                map(solve_for_row, TransformedRows5, NumberSolutionsPerRow5),
                sum(NumberSolutionsPerRow5, ResultP2),
                io.write(ResultP2, !IO),
                io.nl(!IO)
            else
                io.write("fu", !IO)
        )

        ;

        Result = error(Error),
        io.write(Error, !IO)
    )
).
