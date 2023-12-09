% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day07.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module set_ordlist.
:- import_module solutions.
:- import_module string.

:- type rank ---> a; k; q; j; t; nine; eight; seven; six; five; four; three; two; joker.
:- type hand == list(rank).


:- pred parse_p1(list(string)::in, list({hand, int})::out) is det.
parse_p1(In, Out) :- filter_map(parse_row(yes), In, Out).

:- pred parse_p2(list(string)::in, list({hand, int})::out) is det.
parse_p2(In, Out) :- filter_map(parse_row(no), In, Out).

:- pred parse_row(bool::in, string::in, {hand, int}::out) is semidet.
parse_row(IsP1, In, {Hand, Bid}) :- (
    [HandS, BidS] = split_at_char(' ', In),
    Bid = string.det_to_int(BidS),
    filter_map(
        pred(C::in, (R:rank)::out) is semidet :- (
            (
                C = 'A', R = a;
                C = 'K', R = k;
                C = 'Q', R = q;
                C = 'J', IsP1 = yes, R = j;
                C = 'J', IsP1 = no, R = joker;
                C = 'T', R = t;
                C = '9', R = nine;
                C = '8', R = eight;
                C = '7', R = seven;
                C = '6', R = six;
                C = '5', R = five;
                C = '4', R = four;
                C = '3', R = three;
                C = '2', R = two
            )
        ),
        to_char_list(HandS),
        Hand
    )
).

:- pred map_joker(rank::in, rank::out) is multi.
map_joker(joker, a).
map_joker(joker, k).
map_joker(joker, q).
map_joker(joker, t).
map_joker(joker, nine).
map_joker(joker, eight).
map_joker(joker, seven).
map_joker(joker, six).
map_joker(joker, five).
map_joker(joker, four).
map_joker(joker, three).
map_joker(joker, two).
map_joker(In, In).


:- pred strength(hand::in, int::out) is det.
strength(Hand, Res) :- Res = strength(Hand).

:- func strength(hand) = int is det.
strength(Hand) = Score :- (
    sort(Hand, Sorted),
    (
        Sorted = [A, A, A, A, A] -> Score = 7;
        (Sorted = [A, A, A, A, _]; Sorted = [_, A, A, A, A]) -> Score = 6;
        (Sorted = [A, A, A, B, B]; Sorted = [B, B, A, A, A]) -> Score = 5;
        (Sorted = [A, A, A, _, _]; Sorted = [_, _, A, A, A]; Sorted = [_, A, A, A, _]) -> Score = 4;
        (Sorted = [A, A, B, B, _]; Sorted = [A, A, _, B, B]; Sorted = [_, A, A, B, B]) -> Score = 3;
        (Sorted = [A, A, _, _, _]; Sorted = [_, A, A, _, _]; Sorted = [_, _, A, A, _]; Sorted = [_, _, _, A, A]) -> Score = 2;
        Score =  1
    )
).


:- pred possible_strength(hand::in, int::out) is multi.
possible_strength(Hand, Res) :- (
    map(map_joker, Hand, Choice),
    strength(Choice, Res)
).


:- func max_strength(hand) = int is semidet.
max_strength(Hand) = Res :-
    solutions(possible_strength(Hand), Strengths),
    last(Strengths, Res)
    .


:- pred compute(list({hand, int})::in, int::out) is semidet.
compute(Rows, Res) :- (
        map(pred({H, R}::in, {-1 * max_strength(H), H, R}::out) is semidet, Rows, ScoredHands),
        sort(ScoredHands, RSorted),
        reverse(RSorted, Sorted),
        foldl(
            pred({_, _, Score}::in, {TotalIn, MultIn}::in, {TotalOut, MultOut}::out) is det :- (

                MultOut = MultIn + 1,
                TotalOut = TotalIn + Score * MultIn
                ),
            Sorted,
            {0, 1},
            {Res, _}
        )
).

main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day07.txt", Result, !IO),

    (
        Result = ok(Rows),
        (
            if parse_p1(Rows, P1),
               parse_p2(Rows, P2),
               compute(P1, ResP1),
               compute(P2, ResP2)
            then
                io.write(ResP1, !IO),
                io.nl(!IO),
                io.write(ResP2, !IO),
                io.nl(!IO)
            else
                io.write("tough", !IO)
        )
    ;
        Result = error(Error),
        Msg = io.error_message(Error),
        io.write_string(Msg, !IO)
    )
).
