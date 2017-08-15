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
printBoard(Board):- length(Board, Length),Z is Length - 1, printAux(Board, 0, 0, Z).

printAux(Board, Limit, Limit, Limit):- getElement(Board, Limit, Row), getElement(Row, Limit, Elem),
 write(Elem), nl.

printAux(Board, Limit, Ycoord, Limit):- getElement(Board, Ycoord, Row), getElement(Row, Limit, Elem),
 write(Elem), nl, NextColumn is Ycoord + 1, printAux(Board, 0, NextColumn, Limit).

printAux(Board, Xcoord, Ycoord, Limit):- getElement(Board, Ycoord, Row), getElement(Row, Xcoord, Elem),
 write(Elem), write(' '), NextRow is Xcoord + 1, printAux(Board, NextRow, Ycoord, Limit).


%Board Operations
getElement([H|_], 0, H).
getElement([_|T], Pos, Element) :-  Z is Pos - 1, getElement(T, Z, Element).


setElemAt([H|T], (0, Ycoord), Elem, [NewRow|T]):- setEleAux(H, Elem, Ycoord, NewRow).
setElemAt([H|T], (Xcoord, Ycoord), Elem, [H|NT]):- Z is Xcoord - 1, setElemAt(T,(Z, Ycoord), Elem, NT).


setEleAux([_|T], Elem, 0, [Elem|T]).
setEleAux([H|T], Elem, Pos, [H|NT]):- Z is Pos - 1, setEleAux(T, Elem, Z, NT).

generateMine(Limit, Coord):- random_between(0,Limit,Xm), random_between(0,Limit,Ym), Coord = (Xm, Ym).


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
displayBoard(Camp, 9),
setElemAt(Camp, (4, 2), 'V', Answ),
printBoard(Answ), nl, nl.
%halt(0).