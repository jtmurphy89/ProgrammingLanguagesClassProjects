append([], List, List).
append([Head|Tail], Other, [Head|Rest]) :-
    append(Tail, Other, Rest).
