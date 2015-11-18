example(
  node(8,
       node(3,
            node(1, nodenil, nodenil),
            node(6,
                 node(4, nodenil, nodenil),
                 node(7, nodenil, nodenil))),
       node(10,
            nodenil,
            node(14,
                 node(13, nodenil, nodenil),
                 nodenil)))).


allLess(_, []).
allLess(V1, [V2 | Rest]) :-
    V2 > V1,
    allLess(V1, Rest).

allGreater(_, []).
allGreater(V1, [V2 | Rest]) :-
    V2 < V1,
    allGreater(V1, Rest).
 
isBST(nodenil, _, _).

isBST(node(Value, Left, Right), LT, GT) :-
    allLess(Value, LT),
    allGreater(Value, GT),
    isBST(Left, [Value | LT], GT),
    isBST(Right, LT, [Value | GT]).