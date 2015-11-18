simpleDigit(1).

digit(1).
digit(2).
digit(3).

append([], List, List).
append([Head|Tail], Other, [Head|Rest]) :-
    append(Tail, Other, Rest).

