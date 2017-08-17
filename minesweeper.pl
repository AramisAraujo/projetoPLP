:- initialization(main).

% Game Board Tiles

coveredTile('■').
bombTile('💣').
emptyTile('□').
flaggedTile('🏴').

emptyTileNumeric(0).
bombTileNumeric(-1).


%Game and Display board definition
gameBoard(Board, Size):- length(Board,Size), length(Row,Size),
 maplist( =(0), Row),  maplist( =(Row), Board).

displayBoard(Display, Size):- length(Display, Size), length(Row, Size),
coveredTile(X), maplist( =(X), Row), maplist( =(Row), Display).


%Board printing
printBoard(Board):- length(Board, Length),Z is Length - 1, printAux(Board, 0, 0, Z).

printAux(Board, Limit, Limit, Limit):- getElemAt(Board, (Limit, Limit), Elem), write(Elem), nl.

printAux(Board, Limit, Ycoord, Limit):- getElemAt(Board, (Limit, Ycoord), Elem),
 write(Elem), nl, NextColumn is Ycoord + 1, printAux(Board, 0, NextColumn, Limit).

printAux(Board, Xcoord, Ycoord, Limit):- getElemAt(Board, (Xcoord, Ycoord), Elem),
 write(Elem), write(' '), NextRow is Xcoord + 1, printAux(Board, NextRow, Ycoord, Limit).


%Board Operations
getElement([H|_], 0, H).
getElement([_|T], Pos, Element) :-  Z is Pos - 1, getElement(T, Z, Element).


getElemAt(Board, (X,Y), Elem):- getElement(Board, Y, Row), getElement(Row, X, Elem).


setElemAt([H|T], (Xcoord, 0), Elem, [NewRow|T]):- setEleAux(H, Elem, Xcoord, NewRow).
setElemAt([H|T], (Xcoord, Ycoord), Elem, [H|NT]):- Z is Ycoord - 1, setElemAt(T,(Xcoord, Z), Elem, NT).

setEleAux([_|T], Elem, 0, [Elem|T]).
setEleAux([H|T], Elem, Pos, [H|NT]):- Z is Pos - 1, setEleAux(T, Elem, Z, NT).


generateMine(Limit, Coord):- random_between(0,Limit,Xm), random_between(0,Limit,Ym), Coord = (Xm, Ym).


getMines(BoardLimit, Amount, Answer):- length(Mines, Amount), 
    maplist(generateMine(BoardLimit), Mines),  sort(Mines, Answer).


checkCoord((X, Y), Limit):- X @=< Limit, X @>= 0, Y @=< Limit, Y @>= 0.


filterCoords([], _, []).

filterCoords([X|XS], Limit, [X|ZS]):- checkCoord(X, Limit),!, filterCoords(XS, Limit, ZS).

filterCoords([_|XS], Limit, ZS):- filterCoords(XS, Limit, ZS).


filterEmptyTiles([], _, _).
filterEmptyTiles([H|T], GBoard, Answer):- getElemAt(GBoard, H, Elem), Elem =:= 0 -> Answer = [H|Ts], filterEmptyTiles(T, GBoard, Ts);
Answer = Ts, filterEmptyTiles(T, GBoard, Ts).


getAdjCoords([], _, []).
getAdjCoords([H|T], Limit, AllAdjCoords):- getAdjCoords(H, Limit, FirstGroup), getAdjCoords(T, Limit, Coords),
	 flatten([FirstGroup|Coords], Flat), sort(Flat, AllAdjCoords). 
getAdjCoords((X, Y), Limit, AdjacentCoords):- 
	 Coords = [(A, Y), (B, Y), (X, C), (X, D), (A, C), (A, D), (B, C), (B, D)],
	  A is X + 1, B is X - 1, C is Y + 1, D is Y - 1,
	  filterCoords(Coords, Limit, AdjacentCoords).


getAdjEmpty(AdjEmpTiles, (X,Y), GBoard):- length(Board, Len), getAdjCoords((X,Y), Len, Board, Coords),
 getAdjIfEmpty(GBoard, AdjEmpTiles, Coords).

getAdjIfEmpty(_, _, []).

getAdjIfEmpty(GBoard, Results, ToCheck):- length(GBoard, Len),
 filterEmptyTiles(ToCheck, GBoard, CheckFiltered),
 flatten([Results|CheckFiltered], FlatResults), sort(FlatResults, NewResults),
 getAdjCoords(ToCheck, Len, AdjUnchecked), sort(AdjUnchecked, AdjToCheck),
 filterEmptyTiles(AdjToCheck, GBoard, Checking), subtract(Checking, NewResults, NewToCheck),
 getAdjIfEmpty(GBoard, NewResults, NewToCheck).

%openTiles(GBoard, Display, [Hcoords|Tcoords]):-




main :- 

displayBoard(Camp, 9),
setElemAt(Camp, (4, 2), 'V', Answ),
printBoard(Answ), nl, nl,
getMines(8, 9, Answer),
filterCoords([(1, 3), (12, 9), (4, 5)], 8, ValidCoords),
write(ValidCoords),nl,
write(Answer),
getAdjCoords([(0,0), (1,1)], 8, Coords),
write(Coords).

%halt(0).