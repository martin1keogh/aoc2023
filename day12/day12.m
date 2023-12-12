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

:- pred map_condition(char::in, char::out) is multi.
map_condition(I, O) :- I = '?' -> (O = '#'; O = (.)); O = I.

:- pred map_conditions(list(char)::in, list(char)::out) is multi.
map_conditions(LI, LO) :- map(map_condition, LI, LO).

:- pred valid_mapping(list(char)::in, list(int)::in) is semidet.
valid_mapping(Chars, Groups) :-
    % any way to avoid list(char) -> string -> list(char) wankery?
    from_char_list(Chars, String),
    SpringGroups = words_separator(pred(C::in) is semidet :- C \= '#', String),
    SpringGroupsAsCharList = map(string.to_char_list, SpringGroups),
    map(list.length, SpringGroupsAsCharList, Groups).

:- pred solve_for_row({list(char), list(int)}::in, int::out) is det.
solve_for_row({LI, Groups}, NumberSolutions) :-
    promise_equivalent_solutions [NumberSolutions] (
    solutions.unsorted_aggregate(
        map_conditions(LI),
        pred(Res::in, CountIn::in, CountOut::out) is det :- valid_mapping(Res, Groups) -> CountOut = CountIn + 1; CountOut = CountIn,
        0,
        NumberSolutions
    )).

:- pred sum(list(int)::in, int::out) is det.
sum([], 0).
sum([H|T], Out) :- sum(T, Out - H).


main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day12.txt", Result, !IO),

    (
        Result = ok(Rows),
        (
            if map(parse_row, Rows, ParsedRows)
            then
                map(solve_for_row, ParsedRows, NumberSolutionsPerRow),
                sum(NumberSolutionsPerRow, ResultP1),
                io.write(ResultP1, !IO)
            else
                io.write("fu", !IO)
        )

        ;

        Result = error(Error),
        io.write(Error, !IO)
    )
).
