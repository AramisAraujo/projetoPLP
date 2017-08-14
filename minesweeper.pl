:- initialization(main).

% Game Board Tiles

coveredTile('■').
bombTile('💣').
emptyTile('□').
flaggedTile('🏴').

emptyTileN(0).
bombTileN(-1).


%Game and Display board definition
gameBoard(Board, Size):- length(Board,Size), length(Row,Size),
 maplist( =(0), Row),  maplist( =(Row), Board).

displayBoard(Display, Size):- length(Display, Size), length(Row, Size),
coveredTile(X), maplist( =(X), Row), maplist( =(Row), Display).


%Board printing
printBoard(Board):- length(Board, Length), printAux(Board, 1, 1, Length).

printAux(Board, Limit, Limit, Limit):- nth1(Limit, Board, Row), nth1(Limit, Row, Elem),
 write(Elem), nl, !.

printAux(Board, Limit, Ycoord, Limit):- nth1(Limit, Board, Row), nth1(Ycoord, Row, Elem),
 write(Elem), nl, NextColumn is Ycoord + 1, printAux(Board, 1, NextColumn, Limit).

printAux(Board, Xcoord, Ycoord, Limit):- nth1(Limit, Board, Row), nth1(Ycoord, Row, Elem),
 write(Elem), write(' '), NextRow is Xcoord + 1, printAux(Board, NextRow, Ycoord, Limit).


%Board Operations
getElement([H|_], 0, Element):- Element is H.
getElement([_|T], Pos, Element) :-  Z is Pos - 1, getElement(T, Z, Element).







/**----Não consegui compreender ou fazer funcionar :c----

tileHasOpen(Result) :- Result is [H|T].
tileIsBomb(Result) :- Result is [H|T].
tileIsEmpty(Result) :- Result is [H|T].

printingTile(X, Y, Result) :- member((X, Y), tileHasOpen(A)), member((X, Y), tileIsBomb(B)), Result is bombTile(R).

% Defines a empty tile, with yours bombs tips
printingTile(X, Y, Result) :- member((X, Y), tileHasOpen(A)), member((X, Y), tileIsEmpty(B)), Result is emptyTile1(X, Y, 0).

% note that emptytile? verify some position about bomb and call emptytile?+1 to verify another position
(member((X - 1, Y - 1), tileIsBomb(A)) -> emptyTile1(X, Y, N) is emptyTile2(X, Y, S), S is N + 1 ; emptyTile1(X, Y, N) is emptyTile2(X, Y, N)). 
(member((X, Y - 1), tileIsBomb(A)) ->     emptyTile2(X, Y, N) is emptyTile3(X, Y, S), S is N + 1 ; emptyTile2(X, Y, N) is emptyTile3(X, Y, N)).
(member((X + 1, Y - 1), tileIsBomb(A)) -> emptyTile3(X, Y, N) is emptyTile4(X, Y, S), S is N + 1 ; emptyTile3(X, Y, N) is emptyTile4(X, Y, N)).
(member((X - 1, Y), tileIsBomb(A)) ->     emptyTile4(X, Y, N) is emptyTile5(X, Y, S), S is N + 1 ; emptyTile4(X, Y, N) is emptyTile5(X, Y, N)).
(member((X + 1, Y), tileIsBomb(A)) ->     emptyTile5(X, Y, N) is emptyTile6(X, Y, S), S is N + 1 ; emptyTile5(X, Y, N) is emptyTile6(X, Y, N)).
(member((X - 1, Y + 1), tileIsBomb(A)) -> emptyTile6(X, Y, N) is emptyTile7(X, Y, S), S is N + 1 ; emptyTile6(X, Y, N) is emptyTile7(X, Y, N)).
(member((X, Y + 1), tileIsBomb(A)) ->     emptyTile7(X, Y, N) is emptyTile8(X, Y, S), S is N + 1 ; emptyTile7(X, Y, N) is emptyTile8(X, Y, N)).
(member((X + 1, Y + 1), tileIsBomb(A)) -> emptyTile8(X, Y, N) is emptyTile9(X, Y, S), S is N + 1 ; emptyTile8(X, Y, N) is emptyTile9(X, Y, N)).

(emptyTile9(X,Y,N), N > 0 -> emptyTile9(X,Y,N) is A, string_to_atom(A, A1), atom_number(A1, N); emptyTile9(X,Y,N) is nullTile(R)).

printingTile(X, Y, Result) :- Result is coverTile(R).

**/





main :- 

displayBoard(Board, 9),
printBoard(Board),nl,
halt(0).