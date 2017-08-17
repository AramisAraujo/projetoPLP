:- initialization(main).

% Game Board Tiles

coveredTile('A').
bombTile('B').
emptyTile('C').
flaggedTile('D').

itsATileBomb(01).
itsATileBomb(11).
itsATileBomb(21).

itsATileEmpty(0).
itsATileEmpty(00).
itsATileEmpty(10).
itsATileEmpty(20).

itsATileCovered(01).
itsATileCovered(00).
itsATileCovered(0).

itsATileFlagged(20).
itsATileFlagged(21).

itsATileOpened(11).
itsATileOpened(10).

/**emptyTileN(0).
bombTileN(-1).


%Game and Display board definition
gameBoard(Board, Size):- length(Board,Size), length(Row,Size),
 maplist( =(0), Row),  maplist( =(Row), Board).

displayBoard(Display, Size):- length(Display, Size), length(Row, Size),
coveredTile(X), maplist( =(X), Row), maplist( =(Row), Display).
**/

% ----------------- Definicao da funcao que imprime os caracteres ----------------------------------
printingTile(Xcoord, Ycoord, Board, Result) :- nth0(Xcoord, Board, Row), nth0(Ycoord, Row, Elem),
 itsATileOpened(Elem), itsATileBomb(Elem), bombTile(R), Result = (R).

% Defines a empty tile, with yours bombs tips
printingTile(Xcoord, Ycoord, Board, Result) :- nth0(Xcoord, Board, Row), nth0(Ycoord, Row, Elem),
 itsATileOpened(Elem), itsATileEmpty(Elem), printingEmptyTile(Xcoord, Ycoord, Board, 9, R), 
 atom_number(R1,R), string_to_atom(R2,R1), Result = R2.

printingTile(Xcoord, Ycoord, Board, Result) :- nth0(Xcoord, Board, Row), nth0(Ycoord, Row, Elem),
 itsATileCovered(Elem), coveredTile(R), Result = R.

printingTile(Xcoord, Ycoord, Board, Result) :- nth0(Xcoord, Board, Row), nth0(Ycoord, Row, Elem),
 itsATileFlagged(Elem), flaggedTile(R), Result = R.

% ----------------- Fim da definicao da funcao que imprime os caracteres ----------------------------------

% ----------------- Definicao da funcao que imprime a casa vazia ----------------------------------
printingEmptyTile(Xcoord, Ycoord, Board, Limit, Result) :- getAdjacentCoords((Xcoord, Ycoord), Limit, AdjaCo), 
 searchBombs(AdjaCo, Limit, Board, ValidCoords), length(ValidCoords, Length), Result is Length.

searchBombs([], Limit, [C|R], []).

searchBombs([X|XS], Limit, [C|R], [X|ZS]):- checkCoordinate(X, Limit),!, checkIfHasABomb(X, [C|R]), 
 searchBombs(XS, Limit, [C|R], ZS).

searchBombs([_|XS], Limit, [C|R], ZS):- searchBombs(XS, Limit, [C|R], ZS).

checkIfHasABomb((Xcoord, Ycoord), [C|R]):- getElement([C|R], Ycoord, Row), getElement(Row, Xcoord, Elem), 
 itsATileBomb(Elem). 

% ----------------- Fim da definicao da funcao que imprime a casa vazia ----------------------------------
%Board printing
printBoard(Board):- length(Board, Length),Z is Length - 1, printAux(Board, 0, 0, Z).

printAux(Board, Limit, Limit, Limit):- printingTile(Limit, Limit, Board, Elem),
 write(Elem), nl.

printAux(Board, Limit, Ycoord, Limit):- printingTile(Limit, Ycoord, Board, Elem),
 write(Elem), nl, NextColumn is Ycoord + 1, printAux(Board, 0, NextColumn, Limit).

printAux(Board, Xcoord, Ycoord, Limit):- printingTile(Xcoord, Ycoord, Board, Elem),
 write(Elem), write(' '), NextRow is Xcoord + 1, printAux(Board, NextRow, Ycoord, Limit).


%Board Operations
getElement([H|_], 0, H).
getElement([_|T], Pos, Element) :-  Z is Pos - 1, getElement(T, Z, Element).


setElemAt([H|T], (Xcoord, 0), Elem, [NewRow|T]):- setEleAux(H, Elem, Xcoord, NewRow).
setElemAt([H|T], (Xcoord, Ycoord), Elem, [H|NT]):- Z is Ycoord - 1, setElemAt(T,(Xcoord, Z), Elem, NT).

setEleAux([_|T], Elem, 0, [Elem|T]).
setEleAux([H|T], Elem, Pos, [H|NT]):- Z is Pos - 1, setEleAux(T, Elem, Z, NT).

generateMine(Limit, Coord):- random_between(0,Limit,Xm), random_between(0,Limit,Ym), Coord = (Xm, Ym).

getMines(BoardLimit, Amount, Answer):- length(Mines, Amount), 
    maplist(generateMine(BoardLimit), Mines),  sort(Mines, Answer).

checkCoordinate((X, Y), Limit):- X @=< Limit, X @>= 0, Y @=< Limit, Y @>= 0.

filterCoordinates([], Limit, []).

filterCoordinates([X|XS], Limit, [X|ZS]):- checkCoordinate(X, Limit),!, filterCoordinates(XS, Limit, ZS).

filterCoordinates([_|XS], Limit, ZS):- filterCoordinates(XS, Limit, ZS).

getAdjacentCoords((X, Y), Limit, AdjacentCoords):- 
	 Coords = [(A, Y), (B, Y), (X, C), (X, D)], A is X + 1, B is X - 1, C is Y + 1, D is Y - 1,
	 filterCoordinates(Coords, Limit, AdjacentCoords).


% -------------Definicao dos casos de abertura de uma casa------------------------------------------
openTile([H|T], (Xcoord, Ycoord), [H|NT]):- getElement([H|T], Ycoord, Row), getElement(Row, Xcoord, Elem), 
 itsATileCovered(Elem), itsATileBomb(Elem), YNcoord is 8 - Ycoord, setElemAt([H|T], (Xcoord, YNcoord), 11, [H|NT]).

openTile([H|T], (Xcoord, Ycoord), [H|NT]):- getElement([H|T], Ycoord, Row), getElement(Row, Xcoord, Elem), 
 itsATileCovered(Elem), itsATileEmpty(Elem), YNcoord is 8 - Ycoord, setElemAt([H|T], (Xcoord, YNcoord), 10, [H|NT]).

% -------------Fim da definicao dos casos de abertura de uma casa------------------------------------------

% -------------Definicao dos casos de marcacao de uma casa------------------------------------------
flagTile([H|T], (Xcoord, Ycoord), [H|NT]):- getElement([H|T], Ycoord, Row), getElement(Row, Xcoord, Elem), 
 itsATileCovered(Elem), itsATileBomb(Elem), YNcoord is 8 - Ycoord, setElemAt([H|T], (Xcoord, YNcoord), 21, [H|NT]).

flagTile([H|T], (Xcoord, Ycoord), [H|NT]):- getElement([H|T], Ycoord, Row), getElement(Row, Xcoord, Elem), 
 itsATileCovered(Elem), itsATileEmpty(Elem), YNcoord is 8 - Ycoord, setElemAt([H|T], (Xcoord, YNcoord), 20, [H|NT]).

% -------------Fim da definicao dos casos de marcação de uma casa------------------------------------------
    
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

% test functions
fullCoveredBombsBoard(Display, Size):- length(Display, Size), length(Row, Size),
 maplist( =(01), Row), maplist( =(Row), Display).

main :- 
fullCoveredBombsBoard(Camp, 9),
/**setElemAt(Camp, (4, 6), 00, Answ),**/
/**write(Answ), nl, nl,**/
openTile(Camp, (4, 2), Ans),
/**write(Ans), nl, nl,**/
printBoard(Ans), nl, nl,
flagTile(Ans, (1, 8), Answ),
printBoard(Answ), nl, nl,


getMines(9, 9, Answer),
filterCoordinates([(1, 3), (4, 5), (2, 9), (12, 9)], 9, ValidCoords),
write(ValidCoords),nl,
write(Answer).


%halt(0).
