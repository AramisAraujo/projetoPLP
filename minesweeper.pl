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
	Elem == 0 -> Answer = [H|Ts], filterEmptyTiles(T, GBoard, Ts);
	Answer = Ts, filterEmptyTiles(T, GBoard, Ts).


getAdjCoords([], _, []).
getAdjCoords([H|T], Limit, AllAdjCoords):- getAdjCoords(H, Limit, FirstGroup), 
	getAdjCoords(T, Limit, Coords),
	flatten([FirstGroup|Coords], Flat), sort(Flat, AllAdjCoords).

getAdjCoords((X, Y), Limit, AdjacentCoords):- 
	Coords = [(A, Y), (B, Y), (X, C), (X, D), (A, C), (A, D), (B, C), (B, D)],
	A is X + 1, B is X - 1, C is Y + 1, D is Y - 1,
	filterCoords(Coords, Limit, AdjacentCoords).


getAdjEmpty(Coord, GBoard, AdjEmpTiles):- length(GBoard, Len), getAdjCoords(Coord, Len, Adj),
	filterEmptyTiles(Adj, GBoard, Empty), getAdjCoords(Empty, Len, AdjEmp),
	getAdjIfEmpty(GBoard, [Coord|Empty], AdjEmp, AdjEmpTiles).

getAdjIfEmpty(_, PartialRes, [], Answer):- flatten(PartialRes, Answer).
getAdjIfEmpty(GBoard, PartialRes, ToCheck, Results):- 

	filterEmptyTiles(ToCheck, GBoard, Empty),
	flatten([PartialRes|Empty], Temp), sort(Temp, NewRes),

	length(GBoard, Len), getAdjCoords(Empty, Len, TempAdj),
	subtract(TempAdj, NewRes, NewToCheck),
	

	getAdjIfEmpty(GBoard, NewRes, NewToCheck, Results).


openTiles(_, Display, [], Display).
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


applyBombs([], Board, Board):-!. 
applyBombs([Hbomb|Tbomb], GBoard, ResultBoard):-bombTileNumeric(X),length(GBoard, Len),
	setElemAt(GBoard, Hbomb, X, TempBoard), getAdjCoords(Hbomb, Len, AdjCoords),
	addTip(TempBoard, AdjCoords, TempBoard2), applyBombs(Tbomb, TempBoard2, ResultBoard).

addTip(Board, [], Board):-!.
addTip(GBoard, [Hcoord|Tcoords], ResultBoard):- getElemAt(GBoard, Hcoord, X),
	X @>= 0 -> Z is X + 1, setElemAt(GBoard, Hcoord, Z, TempBoard),
	addTip(TempBoard, Tcoords, ResultBoard);
	addTip(GBoard, Tcoords, ResultBoard). 


filterHints([], _, _):-!.
filterHints([H|T], GBoard, Answer):- getElemAt(GBoard, H, Elem), 
	Elem > 0 -> Answer = [H|Ts], filterHints(T, GBoard, Ts);
	Answer = Ts, filterHints(T, GBoard, Ts).


getHintAux([], _, _):-!.
getHintAux([H|T], GBoard, Hints):- length(GBoard, Len), getAdjCoords(H, Len, Adj),
	filterHints(Adj, GBoard, Filtered), Hints = [Filtered|Ts], getHintCoords(T, GBoard, Ts).

getHintCoords(Coords, GBoard, Hints):- getHintAux(Coords, GBoard, Answer),
	flatten(Answer, Flat), sort(Flat, Temp), delete(Temp, (0,0), Hints).

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
	

createBoard(Size, GameBoard):-
	gameBoard(GB, Size),
	Limit is Size - 1, getMines(Limit, Size, BombList),
	applyBombs(BombList, GB, GameBoard).


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


availableTiles(BoardSize, Tiles):- AllTiles = BoardSize*BoardSize,
 	AmountBombs = (BoardSize*BoardSize*10)/100,
 	Tiles = AllTiles - AmountBombs.


main :- 
	gameBoard(GB, 9),
	displayBoard(Dis,9),
	getMines(8, 9, BombList),

	applyBombs(BombList, GB, Board),


	printBoard(Board), nl,
	getAdjEmpty((0,0), Board, Coords), %write(Coords),nl,
	getHintCoords(Coords, Board, Hints),
	flatten([Coords|Hints], F),
	sort(F, ToOpen),
	write(Hints),nl,
	openTiles(Board, Dis, ToOpen, Dis2),
	printBoard(Dis2).
	%halt(0).