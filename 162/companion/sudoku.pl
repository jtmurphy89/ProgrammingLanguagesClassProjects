% A Sudoku solver.  The basic idea is for each position,
% check that it is a digit with `digit`.  Then verify that the digit
% chosen doesn't violate any constraints (row, column, and cube).
% If no constraints were violated, proceed further.  If a constraint
% was violated, then backtrack to the last digit choice and move from
% there (the Prolog engine should handle this for you automatically).
% If we reach the end of the board with this scheme, it means that
% the whole thing is solved.

digit(1).
digit(2).
digit(3).
digit(4).
digit(5).
digit(6).
digit(7).
digit(8).
digit(9).

numBetween(Num, Lower, Upper) :-
        Num >= Lower,
        Num =< Upper.

% cubeBounds: (RowLow, RowHigh, ColLow, ColHigh, CubeNumber)
cubeBounds(0, 2, 0, 2, 0).
cubeBounds(0, 2, 3, 5, 1).
cubeBounds(0, 2, 6, 8, 2).
cubeBounds(3, 5, 0, 2, 3).
cubeBounds(3, 5, 3, 5, 4).
cubeBounds(3, 5, 6, 8, 5).
cubeBounds(6, 8, 0, 2, 6).
cubeBounds(6, 8, 3, 5, 7).
cubeBounds(6, 8, 6, 8, 8).

% Given a board and the index of a column of interest (0-indexed),
% returns the contents of the column as a list.
% columnAsList: (Board, ColumnNumber, AsRow)
columnAsList([], _, []).
columnAsList([Head|Tail], ColumnNum, [Item|Rest]) :-
        nth0(ColumnNum, Head, Item),
        columnAsList(Tail, ColumnNum, Rest).

% given which row and column we are in, gets which cube
% is relevant.  A helper ultimately for `getCube`.
% cubeNum: (RowNum, ColNum, WhichCube)
cubeNum(RowNum, ColNum, WhichCube) :-
        cubeBounds(RowLow, RowHigh, ColLow, ColHigh, WhichCube),
        numBetween(RowNum, RowLow, RowHigh),
        numBetween(ColNum, ColLow, ColHigh).

% Drops the first N elements from a list.  A helper ultimately
% for `getCube`.
% drop: (InputList, NumToDrop, ResultList)
drop([], _, []).
drop(List, 0, List).
drop([_|Tail], Num, Rest) :-
        Num > 0,
        NewNum is Num - 1,
        drop(Tail, NewNum, Rest).

% Takes the first N elements from a list.  A helper ultimately
% for `getCube`.
% take: (InputList, NumToTake, ResultList)
take([], _, []).
take(_, 0, []).
take([Head|Tail], Num, [Head|Rest]) :-
        Num > 0,
        NewNum is Num - 1,
        take(Tail, NewNum, Rest).

% Gets a sublist of a list in the same order, inclusive.
% A helper for `getCube`.
% sublist: (List, Start, End, NewList)
sublist(List, Start, End, NewList) :-
        drop(List, Start, TempList),
        NewEnd is End - Start + 1,
        take(TempList, NewEnd, NewList).

% Given a board and cube number, gets the corresponding cube as a list.
% Cubes are 3x3 portions, numbered from the top left to the bottom right,
% starting from 0.  For example, they would be numbered like so:
%
% 0  1  2
% 3  4  5
% 6  7  8
%
% getCube: (Board, ColumnIndex, ContentsOfCube)
getCube(Board, Number, AsList) :-
        cubeBounds(RowLow, RowHigh, ColLow, ColHigh, Number),
        sublist(Board, RowLow, RowHigh, [Row1, Row2, Row3]),
        sublist(Row1, ColLow, ColHigh, Row1Nums),
        sublist(Row2, ColLow, ColHigh, Row2Nums),
        sublist(Row3, ColLow, ColHigh, Row3Nums),
        append(Row1Nums, Row2Nums, TempRow),
        append(TempRow, Row3Nums, AsList).





 %%   /\_____/\
 %%  ( o     o )
 %%  (   =^=    ) 
 %%  (           )
 %%  (            )
 %%  (             )))))))))))

% Meow


getVariablesHelper([], [], _, 9).
getVariablesHelper([H|T], [[H, Row, Col]|TailsVariables], Row, Col) :- 
        var(H), 
        Col2 is Col + 1, 
        getVariablesHelper(T, TailsVariables, Row, Col2).

getVariablesHelper([H|T], TailsVariables, Row, Col) :- 
        nonvar(H), 
        Col2 is Col + 1,
        getVariablesHelper(T, TailsVariables, Row, Col2).


% takes in a sudoku board and the index 0 and returns a list of tuples [...,[VarName, i,j],...] 
% where i, j are the row and column indices of VarName, respectively.
getVariables([], [] , _).
getVariables([H1|T1], [H|T], Row) :- 
        Row2 is Row + 1,
        getVariablesHelper(H1, H, Row, 0),
        getVariables(T1, T, Row2).



% create column board 
getCols(_, [], 9).
getCols(Board, [H|T], Index) :- 
        columnAsList(Board, Index, H),
        Index1 is Index +1,
        getCols(Board, T, Index1).

% create squares board
getSquares(_, [], 9).
getSquares(Board, [H|T], Index) :- 
        getCube(Board, Index, H),
        Index1 is Index +1,
        getSquares(Board, T, Index1).
        


% get the square number given the indices of a variable
squareNum(I,J,B) :- B is (3*div(I,3) + div(J,3)).

% after we assign a value to a variable: 
checkRow(Board, R) :- 
        nth0(R, Board, R1),
        is_set(R1).

checkCol(X, C) :- 
        nth0(C, X, Col),
        is_set(Col).

checkSquare(R,C, Y) :-
        squareNum(R,C,B),
        nth0(B, Y, Sq),
        is_set(Sq).

% take in row, column, and square boards along with list of [Var, i, j] tuples (necessarily ordered by row),
% assigns a digit to var, checks if the row/col/square corresponding to Var is still a set. Recurses. 
iterativelyAssignValuesToVariables(_ , _, _, []).
iterativelyAssignValuesToVariables(Board, X, Y, [Var, R, C|MoreVars]) :- 
    digit(Var), 
    checkRow(Board, R),
    checkCol(X, C),
    checkSquare(R, C, Y),
    iterativelyAssignValuesToVariables(Board, X, Y, MoreVars).


% takes in a board, generates variable and row/col/square data and iteratively assigns values to the variables 
solve(Board) :- 
        getVariables(Board, VariablesList, 0), 
        flatten(VariablesList, VarList),
        getCols(Board, X, 0),
        getSquares(Board, Y, 0),
        iterativelyAssignValuesToVariables(Board, X, Y, VarList).


% Prints out the given board.
printBoard([]).
printBoard([Head|Tail]) :-
        write(Head), nl,
        printBoard(Tail).

test1(Board) :-
        Board = [[2, _, _, _, 8, 7, _, 5, _],
                 [_, _, _, _, 3, 4, 9, _, 2],
                 [_, _, 5, _, _, _, _, _, 8],
                 [_, 6, 4, 2, 1, _, _, 7, _],
                 [7, _, 2, _, 6, _, 1, _, 9],
                 [_, 8, _, _, 7, 3, 2, 4, _],
                 [8, _, _, _, _, _, 4, _, _],
                 [3, _, 9, 7, 4, _, _, _, _],
                 [_, 1, _, 8, 2, _, _, _, 5]],
        solve(Board),
        printBoard(Board).

test2(Board) :-
        Board = [[_, _, _, 7, 9, _, 8, _, _],
                 [_, _, _, _, _, 4, 3, _, 7],
                 [_, _, _, 3, _, _, _, 2, 9],
                 [7, _, _, _, 2, _, _, _, _],
                 [5, 1, _, _, _, _, _, 4, 8],
                 [_, _, _, _, 5, _, _, _, 1],
                 [1, 2, _, _, _, 8, _, _, _],
                 [6, _, 4, 1, _, _, _, _, _],
                 [_, _, 3, _, 6, 2, _, _, _]],
        solve(Board),
        printBoard(Board).

test3(Board) :-
        Board = [[8, _, _, _, _, _, _, _, _],
                 [_, _, 3, 6, _, _, _, _, _],
                 [_, 7, _, _, 9, _, 2, _, _],
                 [7, _, _, _, 2, _, _, _, _],
                 [_, 5, _, _, _, 7, _, _, _],
                 [_, _, _, 1, _, _, _, 3, _],
                 [_, _, 1, _, _, _, _, 6, 8],
                 [_, _, 8, 5, _, _, _, 1, _],
                 [_, 9, _, _, _, _, 4, _, _]],
        getVariables(Board, X, 0),
        flatten(X, B).

