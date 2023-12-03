% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day3.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module bool.
:- import_module ranges.
:- import_module pair.
:- import_module multi_map.
:- import_module map.


:- type indexed_value ---> iv(value::int, start::int, end::int, row::int).

:- pred parse_line(int::in, list(char)::in, int::in, int::in, int::in, list(indexed_value)::out) is det.

parse_line(V, [], S, E, Row, R) :- (
  if S < E
  then (
    R = [iv(V, S, E, Row)]
  )
  else (
    R = []
  )
).

parse_line(V, [C | XS], S, E, Row, R) :- (
  if string.to_int(char_to_string(C), ParseR)
  then (
    parse_line(V * 10 + ParseR, XS, S, E+1, Row, R)
  )
  else (
    if S < E then (
      append([iv(V, S, E, Row)], Acc, R),
      parse_line(0, XS, E+1, E+1, Row, Acc)
    )
    else
      parse_line(0, XS, E+1, E+1, Row, R)
  )
).


:- pred is_symbol(list(list(char))::in, int::in, int::in) is semidet.
is_symbol(Grid, X, Y) :- (
  DotAndDigits = ['.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],

  list.index0(Grid, Y, Row),
  list.index0(Row, X, Elem),
  (if list.member(Elem, DotAndDigits) then fail else true)
).


:- pred has_adjacent_symbol(indexed_value::in, list(list(char))::in, bool::out) is det.
has_adjacent_symbol(iv(_, S, E, Row), Grid, R) :- (
  ToCheck = condense(
    map(
      func(X::in) = (R0::out) is det :- R0 = map(
        func(Y::in) = (R1::out) is det :- R1 = {X, Y},
        to_sorted_list(range(Row - 1, Row + 1))
      ),
      to_sorted_list(range(S - 1, E))
    )
  ),
  (
    if any_true(pred({X, Y}::in) is semidet :- is_symbol(Grid, X, Y), ToCheck)
    then R=yes
    else R=no
  )
).

:- pred is_star(list(list(char))::in, int::in, int::in) is semidet.
is_star(Grid, X, Y) :- (
  list.index0(Grid, Y, Row),
  list.index0(Row, X, '*')
).


:- pred iv_with_adjacent_star(indexed_value::in, list(list(char))::in, pair({int, int}, int)::out) is semidet.
iv_with_adjacent_star(IV @ iv(_, S, E, Row), Grid, R) :- (
  ToCheck = condense(
    map(
      func(X::in) = (R0::out) is det :- R0 = map(
        func(Y::in) = (R1::out) is det :- R1 = {X, Y},
        to_sorted_list(range(Row - 1, Row + 1))
      ),
      to_sorted_list(range(S - 1, E))
    )
  ),
  (
    if find_first_match(pred({X, Y}::in) is semidet :- is_star(Grid, X, Y), ToCheck, FM)
    then R=pair(FM, IV^value)
    else fail
  )
).

main(!IO) :- (
  io.read_named_file_as_lines("/tmp/03.txt", Result, !IO),

  (
    Result = ok(Lines),
    list.map(pred(S::in, O::out) is det :- string.to_char_list(S, O), Lines, Grid),

    % Parsing
    list.foldl(
      pred(Current::in, Acc::in, Acc1::out) is det :- (
        if {_, I} = list.head(Acc) then list.append([{Current, I+1}], Acc, Acc1)
        else list.append([], [{Current, 0}], Acc1)
      ),
      Lines,
      [],
      LinesWithIndex
    ),

    list.map(
      pred({Line, I}::in, O::out) is det :- parse_line(0, to_char_list(Line), 0, 0, I, O),
      LinesWithIndex,
      Parsed
    ),

    list.condense(Parsed, IndexedValues),


    % Part 1
    list.filter(
      pred(IV::in) is semidet :- has_adjacent_symbol(IV, Grid, yes),
      IndexedValues,
      IndexedValuesWithAdjacentSymbols
    ),

    foldl(
      pred(IV1::in, SumAcc::in, V::out) is det :- V = IV1^value + SumAcc,
      IndexedValuesWithAdjacentSymbols,
      0,
      Sum
    ),

    io.format("Part 1: %d", [i(Sum)], !IO),
    io.nl(!IO),


    % Part 2
    list.filter_map(
      pred(IV::in, OO::out) is semidet :- iv_with_adjacent_star(IV, Grid, OO),
      IndexedValues,
      IndexedValuesWithAdjacentStars
    ),

    io.write(IndexedValuesWithAdjacentStars, !IO),

    multi_map.from_flat_assoc_list(
      IndexedValuesWithAdjacentStars,
      StarToValues
    ),

    map.filter_map_values_only(
      pred(([IV1, IV2])::in, OO::out) is semidet :- OO = IV1 * IV2,
      StarToValues,
      Products
    ),

    foldl_values(
      pred(Product1::in, SumAcc::in, V::out) is det :- V = Product1 + SumAcc,
      Products,
      0,
      SumProduct
    ),

    io.format("Part 2: %d", [i(SumProduct)], !IO),
    io.nl(!IO)
  ;
    Result = error(Error),
    Msg = io.error_message(Error),
    io.write_string(Msg, !IO)
  )
).
