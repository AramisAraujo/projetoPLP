:- initialization(main).
main :- write('Hello World!').


coverTile(Result) :- Result is "A".
bombTile(Result) :- Result is "B".
nullTile(Result) :- Result is "C".

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

