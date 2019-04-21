adjacent(X, Y, X1, Y):- X1 is X+1.
adjacent(X, Y, X1, Y):- X1 is X-1.
adjacent(X, Y, X, Y1):- Y1 is Y+1.
adjacent(X, Y, X, Y1):- Y1 is Y-1.

dirty(Dirt, N, M):- N * M * 60 / 100 =< Dirt.
is_dirt(X, Y, Dirtlist):- member((X, Y), Dirtlist).
inside_enviroment(X, Y, N, M):-
	X>=0,X<N, 
	Y>=0,Y<M.
