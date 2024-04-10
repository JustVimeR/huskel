
is_positive(Number) :- Number > 0.

count_positives([], 0).
count_positives([Head|Tail], Count) :-
    is_positive(Head),
    !, count_positives(Tail, TailCount),
    Count is TailCount + 1.
count_positives([_|Tail], Count) :-
    count_positives(Tail, Count).

find_positive_positions(List, Positions) :- find_positive_positions(List, 1, Positions).

find_positive_positions([], _, []).
find_positive_positions([Head|Tail], Index, [Index|PositionsTail]) :-
    is_positive(Head),
    !, NextIndex is Index + 1,
    find_positive_positions(Tail, NextIndex, PositionsTail).
find_positive_positions([_|Tail], Index, Positions) :-
    NextIndex is Index + 1,
    find_positive_positions(Tail, NextIndex, Positions).

example :-
    List = [3, -2, 3, 2, 3, 1, 2, -4],
    count_positives(List, Count),
    find_positive_positions(List, Positions),
    write('Кількість додатних чисел: '), write(Count), nl,
    write('Позиції додатних чисел: '), write(Positions), nl.
