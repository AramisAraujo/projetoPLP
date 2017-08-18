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


%Board Printing
printBoard(Board):- length(Board, Length),Z is Length - 1, findall(Num, between(0, Z, Num), Nums),
					write("                "),printIconsAux(Nums),nl,
					length(Icons, Length), maplist(=("▼"), Icons), 
					write("                "),printIconsAux(Icons),nl,nl,
					printAux(Board, 0, 0, Z).

printIconsAux([]).
printIconsAux([H|T]):- write(H), write("   "), printIconsAux(T).
					

printAux(Board, 0, Ycoord, Limit):- getElemAt(Board, (Xcoord, Ycoord), Elem), 
	write('           '),write(Ycoord),write(' ▶  '),
	write(Elem), write('   '), NextRow is Xcoord + 1, printAux(Board, NextRow, Ycoord, Limit).


printAux(Board, Limit, Limit, Limit):- getElemAt(Board, (Limit, Limit), Elem), write(Elem), nl.

printAux(Board, Limit, Ycoord, Limit):- getElemAt(Board, (Limit, Ycoord), Elem),
	write(Elem), nl, nl, NextColumn is Ycoord + 1, printAux(Board, 0, NextColumn, Limit).

printAux(Board, Xcoord, Ycoord, Limit):- getElemAt(Board, (Xcoord, Ycoord), Elem),
	write(Elem), write('   '), NextRow is Xcoord + 1, printAux(Board, NextRow, Ycoord, Limit).



%Board Operations
getElement([H|_], 0, H):- !.
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
filterEmptyTiles([H|T], GBoard, Answer):- getElemAt(GBoard, H, Elem), 
	Elem =:= 0 -> Answer = [H|Ts], filterEmptyTiles(T, GBoard, Ts);
	Answer = Ts, filterEmptyTiles(T, GBoard, Ts).


getAdjCoords([], _, []).
getAdjCoords([H|T], Limit, AllAdjCoords):- getAdjCoords(H, Limit, FirstGroup), 
	getAdjCoords(T, Limit, Coords),
	flatten([FirstGroup|Coords], Flat), sort(Flat, AllAdjCoords).

getAdjCoords((X, Y), Limit, AdjacentCoords):- 
	Coords = [(A, Y), (B, Y), (X, C), (X, D), (A, C), (A, D), (B, C), (B, D)],
	A is X + 1, B is X - 1, C is Y + 1, D is Y - 1,
	filterCoords(Coords, Limit, AdjacentCoords).


getAdjEmpty((X,Y), GBoard, AdjEmpTiles):- length(GBoard, Len), getAdjCoords((X,Y), Len, Coords),
 getAdjIfEmpty(GBoard, Coords, [(X,Y)|AdjEmpTiles]).

getAdjIfEmpty(_, [], Results).

getAdjIfEmpty(GBoard, ToCheck, Results):- length(GBoard, Len),
	filterEmptyTiles(ToCheck, GBoard, CheckFiltered),
	flatten([Results|CheckFiltered], FlatResults), sort(FlatResults, NewResults),

	getAdjCoords(ToCheck, Len, AdjUnchecked), sort(AdjUnchecked, AdjToCheck),
	filterEmptyTiles(AdjToCheck, GBoard, Checking), subtract(Checking, NewResults, NewToCheck),

	getAdjIfEmpty(GBoard, NewToCheck, NewResults).

openTiles(_,_, [],_).
openTiles(GBoard, Display, [Hcoords|Tcoords], NewDisplay):- getElemAt(Display, Hcoords, ElemD),
	coveredTile(X), 
	X \== ElemD -> openTiles(GBoard, Display, Tcoords, NewDisplay);

	getElemAt(GBoard, Hcoords, ElemB),
	emptyTileNumeric(ElemB) -> emptyTile(Y), setElemAt(Display, Hcoords, Y, TempDisplay),
	openTiles(GBoard, TempDisplay, Tcoords, NewDisplay);

	getElemAt(GBoard, Hcoords, ElemB),
	ElemB > 0 -> setElemAt(Display, Hcoords, ElemB, TempDisplay), 
	openTiles(GBoard, TempDisplay, Tcoords, NewDisplay);

	getElemAt(GBoard, Hcoords, ElemB),
	bombTileNumeric(ElemB) -> bombTile(Z), setElemAt(Display, Hcoords, Z, TempDisplay),
	openTiles(GBoard, TempDisplay, Tcoords, NewDisplay).


flagTile(DisplayBoard, Coord, NewDisplay):-
	getElemAt(DisplayBoard, Coord, Elem), coveredTile(Covered),
	Elem \= Covered -> write("You can't flag this tile!"), NewDisplay = DisplayBoard, nl;
	flaggedTile(Flag), setElemAt(DisplayBoard, Coord, Flag, NewDisplay).

applyBombs([T], GBoard, ResultBoard):- bombTileNumeric(X),length(GBoard, Len),
	setElemAt(GBoard, T, X, TempBoard), getAdjCoords(T, Len, AdjCoords),
	addTip(TempBoard, AdjCoords, ResultBoard).


applyBombs([Hbomb|Tbomb], GBoard, ResultBoard):-bombTileNumeric(X),length(GBoard, Len),
	setElemAt(GBoard, Hbomb, X, TempBoard), getAdjCoords(Hbomb, Len, AdjCoords),
	addTip(TempBoard, AdjCoords, TempBoard2), applyBombs(Tbomb, TempBoard2, ResultBoard).

addTip(GBoard, [T], ResultBoard):- getElemAt(GBoard, T, X),
	X @>= 0 -> Z is X + 1, setElemAt(GBoard, T, Z, ResultBoard). 

addTip(GBoard, [Hcoord|Tcoords], ResultBoard):- getElemAt(GBoard, Hcoord, X),
	X @>= 0 -> Z is X + 1, setElemAt(GBoard, Hcoord, Z, TempBoard),
	addTip(TempBoard, Tcoords, ResultBoard);
	X @< 0 -> addTip(GBoard, Tcoords, ResultBoard). 


%getHintCoords

%User interaction
header(Board, Option) :- 
	nl, nl, nl,
	write("			## Minesweeper##"), nl, nl,
	write("		1. Open Tile"), nl,
	write("		2. Flag Tile"), nl,
	write("		3. Exit Game"), nl, nl,
	printBoard(Board), nl, nl,
	getOption(Option).


getOption(Option):-
	write("Please, type a valid option: "), readNum(Op),
	Op @> 0 ->
		Option = Op;
		write("	Your option is not valid!"),nl, getOption(Option).

getCoords(Size, (X, Y)):-
	write("Please, type your coordinates"),nl,
	write("		Row: "), readNum(Xi),
	write("		Column: "), readNum(Yi),
	checkCoord((Xi, Yi), Size) -> X = Xi, Y = Yi;
	write("Your coordinates are not valid! "), nl, getCoords(Size, (X, Y)).


readNum(X):- read_line_to_codes(user_input,A2),
	string_to_atom(A2,A1),
	atom_number(A1,X).
	
applyBombs([T], GBoard, ResultBoard):- bombTileNumeric(X),length(GBoard, Len),
    setElemAt(GBoard, T, X, TempBoard), getAdjCoords(T, Len, AdjCoords),
    addTip(TempBoard, AdjCoords, ResultBoard).
	
applyBombs([Hbomb|Tbomb], GBoard, ResultBoard):-bombTileNumeric(X),length(GBoard, Len),
    setElemAt(GBoard, Hbomb, X, TempBoard), getAdjCoords(Hbomb, Len, AdjCoords),
    addTip(TempBoard, AdjCoords, TempBoard2), applyBombs(Tbomb, TempBoard2, ResultBoard).
	
	
addTip(GBoard, [T], ResultBoard):- getElemAt(GBoard, T, X),
    X @>= 0 -> Z is X + 1, setElemAt(GBoard, T, Z, ResultBoard). 
addTip(GBoard, [Hcoord|Tcoords], ResultBoard):- getElemAt(GBoard, Hcoord, X),
    X @>= 0 -> Z is X + 1, setElemAt(GBoard, Hcoord, Z, TempBoard),
    addTip(TempBoard, Tcoords, ResultBoard);
    X @< 0 -> addTip(GBoard, Tcoords, ResultBoard). 

createBoard(Size, GameBoard):-
	gameBoard(GB, Size),
	Limit is Size - 1, getMines(Limit, Size, BombList),
	applyBombs(BombList, GB, GameBoard).

/** Ainda não funciona :c


game(GameBoard, DisplayBoard):-
	header(DisplayBoard, Option), length(GameBoard, Size),
	=(Option, 1) ->
		getCoords(Size, Coord),
		flagTile(DisplayBoard, Coord, NewDisplay),
		game(GameBoard, NewDisplay);
	=(Option, 2) ->
		getCoords(Size, Coord),
		write("Not Yet"),
		game(GameBoard, DisplayBoard);
	=(Option, 3) ->
		write("Game Over!!"), nl, nl, halt.

**/




main :- 
	createBoard(9, GB),
	displayBoard(Camp, 9),
	game(GB, Camp).

/**
	gameBoard(GB, 9),
	printBoard(GB),nl,
	getMines(8, 9, BombList),
	write(BombList),nl,
	%addTip(GB, [(5,5),(6,6),(7,7)], Result),
	applyBombs(BombList, GB, Result),
	printBoard(Result),
	getAdjEmpty((8,3), Coords, Result), write(Coords).

	displayBoard(Camp, 9),
	bombTile(X),
	setElemAt(Camp, (4, 2), X, Answ),
	header(Answ), nl, nl,
	getOption(Option),nl, nl,
	getCoords(9, (A, B)),
	filterCoords([(1, 3), (12, 9), (4, 5)], 8, ValidCoords),
	write(ValidCoords),nl,
	write(Answer),
	getAdjCoords([(0,0), (1,1)], 8, Coords),
	write(Coords).
**/
	%halt(0).