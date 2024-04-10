
is_valley([X,Y,Z|_]) :- X > Y, Y < Z.

split_list([], [], []).
split_list([X], [X], []).
split_list([X,Y|T], [X|P], R) :-
    not(is_valley([X,Y|T])),
    split_list([Y|T], P, R).
split_list([X,Y|T], [X,Y], [Y|T]) :-
    is_valley([X,Y|T]).

split_by_valleys(L, R) :- split_by_valleys_helper(L, [], R).
split_by_valleys_helper([], Acc, Acc).
split_by_valleys_helper(L, Acc, R) :-
    split_list(L, P, S),
    append(Acc, [P], NewAcc),
    split_by_valleys_helper(S, NewAcc, R).

example :-
    L = [5, 4, 2, 8, 3, 1, 6, 9, 5],
    split_by_valleys(L, R),
    print(R).


