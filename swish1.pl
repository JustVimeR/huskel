
filter_elements(List, Result) :-
    filter_elements(List, Result, 1). 

filter_elements([], [], _). 

filter_elements([H|T], [H|RT], Index) :-
    H =:= Index, 
    NextIndex is Index + 1, 
    filter_elements(T, RT, NextIndex). 
    
filter_elements([_|T], RT, Index) :-
    NextIndex is Index + 1,
    filter_elements(T, RT, NextIndex). 



