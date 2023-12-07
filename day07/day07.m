% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day07.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module int.
:- import_module list.
:- import_module ranges.
:- import_module solutions.
:- import_module string.

:- type rank ---> a; k; q; j; t; nine; eight; seven; six; five; four; three; two.
:- type hand == list(rank).

:- pred parse_p1(list(string)::in, list({hand, int})::out) is semidet.
parse_p1(In, Out) :- filter_map(parse_row, In, Out).


:- pred parse_row(string::in, {hand, int}::out) is semidet.
parse_row(In, {Hand, Bid}) :- (
    [HandS, BidS] = split_at_char(' ', In),
    Bid = string.det_to_int(BidS),
    filter_map(
        pred(C::in, (R:rank)::out) is semidet :- (
            (
                C = 'A', R = a;
                C = 'K', R = k;
                C = 'Q', R = q;
                C = 'J', R = j;
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


:- func strength(hand) = int is semidet.
strength(Hand) = Score :- (

/*:- pred strength(hand::in, int::out) is semidet.*/
/*strength(Hand, Score) :- (*/
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


main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day07.txt", Result, !IO),

    (
        Result = ok(Rows),
        (
            if parse_p1(Rows, P1),
                map(pred({H, R}::in, {-1 * strength(H), H, R}::out) is semidet, P1, ScoredHands),
                sort(ScoredHands, RSorted),
                reverse(RSorted, Sorted),
                foldl(
                    pred({_, _, Score}::in, {TotalIn, MultIn}::in, {TotalOut, MultOut}::out) is det :- (

                      MultOut = MultIn + 1,
                      TotalOut = TotalIn + Score * MultIn
                    ),
                    Sorted,
                    {0, 1},
                    {ResP1, _}
                )
            then
                io.write(ResP1, !IO),
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
