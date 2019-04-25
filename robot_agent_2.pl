% Helpers Predicates
adjacent(X, Y, X1, Y):- X1 is X+1.
adjacent(X, Y, X1, Y):- X1 is X-1.
adjacent(X, Y, X, Y1):- Y1 is Y+1.
adjacent(X, Y, X, Y1):- Y1 is Y-1.
adjacent(X, Y, X1, Y1):- X1 is X+1, Y1 is Y+1.
adjacent(X, Y, X1, Y1):- X1 is X-1, Y1 is Y+1.
adjacent(X, Y, X1, Y1):- X1 is X+1, Y1 is Y-1.
adjacent(X, Y, X1, Y1):- X1 is X-1, Y1 is Y-1.

get_direction([(X, Y), (X1, Y) |_], down):-X1 is X+1.
get_direction([(X, Y), (X1, Y) |_], up):- X1 is X-1.
get_direction([(X, Y), (X, Y1) |_], left):- Y1 is Y-1.
get_direction([(X, Y), (X, Y1) |_], right):- Y1 is Y+1.
get_direction([(X, Y), (X1, Y1) |_], downleft):- X1 is X+1, Y1 is Y-1.
get_direction([(X, Y), (X1, Y1) |_], downright):- X1 is X+1, Y1 is Y+1.
get_direction([(X, Y), (X1, Y1) |_], upleft):- X1 is X-1, Y1 is Y-1.
get_direction([(X, Y), (X1, Y1) |_], upright):- X1 is X-1, Y1 is Y+1.

is_dirt(X, Y, Dirtlist):- member((X, Y), Dirtlist).
inside_enviroment(X, Y, N, M):-
	X>=0,X<N, 
	Y>=0,Y<M.

% temporary almost dirty definition
is_very_dirty(Dirts, _, _, N, M):- 
	length(Dirts, Size), 
	N * M * 60 / 100 =< Size + N + M.

%=====================================================================+

% BFS Helpers Predicates
visited(Node, [LastPath|[]]):-member(Node, LastPath), !.
visited(Node, [Path|Paths]):-
	member(Node, Path);
	visited(Node, Paths).

expand([[(X, Y)|Path]|Paths], N, M, Obstacles, Extended):-
	findall([(U,V), (X, Y)|Path], 
	(adjacent(X, Y, U, V),
	inside_enviroment(U, V, N, M),
	not(member((U, V), Obstacles)),
not(visited((U,V), [[(X, Y)|Path]|Paths])))
, Extended).

% BFS(PathsQueue, Rows, Cols, Obstacles, Goals, Solution)
% returns the first shortest path to a goal from the list of goals
bfs([[Node|Path]|_], _, _, _, GoalsList, Solution):-
	member(Node, GoalsList),
	reverse([Node|Path], Solution),
	!.
bfs(PathsQueue, N, M, Obstacles, GoalsList, Solution):-
	PathsQueue = [_|Paths],
	expand(PathsQueue, N, M, Obstacles, Extended),
	append(Paths, Extended, Path1),
	bfs(Path1, N, M, Obstacles, GoalsList, Solution).

%=====================================================================+

% Robot Agent:
% next_move((X, Y), N, M, Carrying, Dirts, Childs, Obstacles, Corrals, [Move]):-