% vim: ft=mercury ff=unix ts=4 sw=4 et

:- module day23.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.
:- implementation.

:- import_module array2d.
:- import_module char.
:- import_module digraph.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pretty_printer.
:- import_module ranges.
:- import_module require.
:- import_module set.
:- import_module sparse_bitset.
:- import_module solutions.
:- import_module string.

:- pred build_graph(array2d(char)::in, digraph({int, int})::out) is det.
build_graph(Grid, Graph) :-
    bounds(Grid, YMax, XMax),
    foldl(
        pred(Y::in, WG::in, WGOut::out) is det :- (
            foldl(
                pred(X::in, !.SubWG::in, !:SubWG::out) is det :- (
                    unsafe_lookup(Grid, Y, X, E),
                    Current = {Y, X},
                    (
                        E = (<), in_bounds(Grid, Y, X - 1) -> add_vertices_and_edge(Current, {Y, X - 1}, !SubWG)
                        ;
                        E = (^), in_bounds(Grid, Y - 1, X) -> add_vertices_and_edge(Current, {Y - 1, X}, !SubWG)
                        ;
                        E = (>), in_bounds(Grid, Y, X + 1) -> add_vertices_and_edge(Current, {Y, X + 1}, !SubWG)
                        ;
                        E = (v), in_bounds(Grid, Y + 1, X) -> add_vertices_and_edge(Current, {Y + 1, X}, !SubWG)
                        ;
                        E = (.) ->
                            some[EE] (in_bounds(Grid, Y - 1, X), lookup(Grid, Y - 1, X, EE), (EE \= '#', EE \= 'v') -> add_vertices_and_edge(Current, {Y - 1, X}, !SubWG); !:SubWG = !.SubWG),
                            some[EE] (in_bounds(Grid, Y + 1, X), lookup(Grid, Y + 1, X, EE), (EE \= '#', EE \= (^)) -> add_vertices_and_edge(Current, {Y + 1, X}, !SubWG); !:SubWG = !.SubWG),
                            some[EE] (in_bounds(Grid, Y, X - 1), lookup(Grid, Y, X - 1, EE), (EE \= '#', EE \= (>)) -> add_vertices_and_edge(Current, {Y, X - 1}, !SubWG); !:SubWG = !.SubWG),
                            some[EE] (in_bounds(Grid, Y, X + 1), lookup(Grid, Y, X + 1, EE), (EE \= '#', EE \= (<)) -> add_vertices_and_edge(Current, {Y, X + 1}, !SubWG); !:SubWG = !.SubWG)
                        ;
                        !:SubWG = !.SubWG
                    )
                ),
                range(0, XMax),
                WG,
                WGOut
            )
        ),
        range(0, YMax),
        digraph.init,
        Graph
    ).

:- pred compare_by_length(list(T)::in, list(T)::in, comparison_result::uo) is det.
compare_by_length(L1, L2, R) :-
    length(L1, LL1),
    length(L2, LL2),
    compare(R, LL1, LL2).

:- pred path(digraph(T)::in, digraph_key(T)::in, digraph_key(T)::in, set(digraph_key(T))::in, list(digraph_key(T))::out) is nondet.
path(_, End, End, _, [End]).
path(Graph, Start, End, Seen, Path) :-
    lookup_from(Graph, Start, ReachableFromStart),
    set.member(Next, ReachableFromStart),
    not set.member(Next, Seen),
    insert(Next, Seen, SeenNext),
    path(Graph, Next, End, SeenNext, PathNext),
    Path = [Start] ++ PathNext.

:- pred longuest_path(digraph(T)::in, digraph_key(T)::in, digraph_key(T)::in, list(T)::out) is det.
longuest_path(Graph, StartK, EndK, Path) :-
    solutions(path(Graph, StartK, EndK, set.make_singleton_set(StartK)), PathsK),
    sort(compare_by_length, PathsK, SortedPathsK),
    LonguestPathK = det_head(reverse(SortedPathsK)),
    map(lookup_vertex(Graph), LonguestPathK, Path).

:- pred longuest_path_p2(digraph(T)::in, digraph_key(T)::in, digraph_key(T)::in, list(T)::out) is cc_multi.
longuest_path_p2(Graph, StartK, EndK, Path) :-
    reduced(Graph, ReducedGraph, Correspondance),
    RCorrepondance = reverse_map(Correspondance),
    sc(ReducedGraph, SCRGraph),  % symmetrical closure

    unsorted_aggregate(
        path(SCRGraph, lookup(Correspondance, StartK), lookup(Correspondance, EndK), set.make_singleton_set(lookup(Correspondance, StartK))),
        pred(PathK::in, LonguestPathKOpt::in, yes({NewLonguestPathK, NewLength})::out) is det :- (
            ExpandedCliques = map(lookup(RCorrepondance), PathK),
            condense(map(to_sorted_list, ExpandedCliques), ExpandedPathK),
            length(ExpandedPathK, Length + 1),  % +1 to still remove the [End] link, same as P1
            (
                LonguestPathKOpt = no, NewLonguestPathK = ExpandedPathK, NewLength = Length
                ;
                LonguestPathKOpt = yes({CurrentLonguestPathK, CurrentLength}),
                (
                    CurrentLength >= Length ->
                    NewLonguestPathK = CurrentLonguestPathK, NewLength = CurrentLength
                    ;
                    trace [io(!IO)] (io.write(NewLength, !IO), io.nl(!IO)),
                    NewLonguestPathK = ExpandedPathK, NewLength = Length)
            )
        ),
        no,
        WinningPathAndLength
    ),

    (
        WinningPathAndLength = yes({LonguestPathK, _})
        ;
        WinningPathAndLength = no, error("Too bad")
    ),
    map(lookup_vertex(Graph), LonguestPathK, Path).


main(!IO) :- (
    io.read_named_file_as_lines("/tmp/day23.txt", Result, !IO),

    (
        Result = ok(Rows),
        Grid = array2d(list.map(to_char_list, Rows)),
        bounds(Grid, YMax, XMax),
        StartV = {0, 1},
        EndV = {YMax - 1, XMax - 2},
        lookup_key(Graph, StartV, StartK),
        lookup_key(Graph, EndV, EndK),
        build_graph(Grid, Graph),

        longuest_path(Graph, StartK, EndK, LonguestPath),
        length(LonguestPath, P1 + 1),
        io.write({"P1:", P1}, !IO), io.nl(!IO),

        io.write("This should take about 10 to 15 minutes. Intermediate results for P2 will be printed as a new longest path is found.", !IO),
        io.nl(!IO),

        longuest_path_p2(Graph, StartK, EndK, LonguestPathP2),
        length(LonguestPathP2, P2 + 1),

        io.write({"P2:", P2}, !IO),
        io.nl(!IO)

        ;

        Result = error(Error),
        io.write(Error, !IO)
    )
).
