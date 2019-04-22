% Helpers Predicates
adjacent(X, Y, X1, Y):- X1 is X+1.
adjacent(X, Y, X1, Y):- X1 is X-1.
adjacent(X, Y, X, Y1):- Y1 is Y+1.
adjacent(X, Y, X, Y1):- Y1 is Y-1.

get_direction([(X, Y), (X+1, Y) |_], down).
get_direction([(X, Y), (X-1, Y) |_], up).
get_direction([(X, Y), (X, Y-1) |_], left).
get_direction([(X, Y), (X, Y+1) |_], right).

is_dirt(X, Y, Dirtlist):- member((X, Y), Dirtlist).
inside_enviroment(X, Y, N, M):-
	X>=0,X<N, 
	Y>=0,Y<M.

% temporary almost dirty definition
is_very_dirty(Dirts, Childs, Obstacles, N, M):- 
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

%	Dirt:
% 		Not carrying a child, enviroment is very dirty: Find dirt to clean
next_move((X, Y), N, M, Carrying, Dirts, Childs, Obstacles, _, [Move]):-
	Carrying = false,
	not(is_very_dirty(Dirts, Childs, Obstacles, N, M)),
	bfs([[(X, Y)]], N, M, Obstacles, Childs, Path),
	get_direction(Path, Move),
	!.

% 		Not carrying a child, enviroment is very dirty, standing on dirt: Clean the dirt
next_move((X, Y), N, M, Carrying, Dirts, Childs, Obstacles, _, [clean]):-
	Carrying = false,
	is_very_dirty(Dirts, Childs, Obstacles, N, M),
	member((X, Y), Dirts),
	!.


% 	Child:
%		Caryying a child, enviroment isnt very dirty: Move towards closest corral to drop child (1 Step)
next_move((X, Y), N, M, Carrying, Dirts, Childs, Obstacles, Corrals, [Move]):-
	Carrying = true,
	not(is_very_dirty(Dirts, Childs, Obstacles, N, M)),
	append(Obstacles, Childs, RobotObstacles),
	bfs([[(X, Y)]], N, M, RobotObstacles, Corrals, [S, T|PathTail]),
	member(T, Corrals),
	get_direction([S, T|PathTail], Move),
	!.

%		Caryying a child, enviroment isnt very dirty: Move towards closest corral to drop child (2 Step)
next_move((X, Y), N, M, Carrying, Dirts, Childs, Obstacles, Corrals, [Move1, Move2]):-
	Carrying = true,
	not(is_very_dirty(Dirts, Childs, Obstacles, N, M)),
	append(Obstacles, Childs, RobotObstacles),
	bfs([[(X, Y)]], N, M, RobotObstacles, Corrals, [S, T, Y|PathTail]),
	get_direction([S, T, Y|PathTail], Move1),
	get_direction([T, Y|PathTail], Move2),
	!.

% 		Carrying a child, standing at corral: Drop child
next_move((X, Y), _, _, Carrying, _, _, _, Corrals, [drop]):-
	Carrying = true,
	member((X, Y), Corrals),
	!.

% 		Not carrying a child, enviroment isnt very dirty: Move towards closest child to carry
next_move((X, Y), N, M, Carrying, Dirts, Childs, Obstacles, _, [Move]):-
	Carrying = false,
	not(is_very_dirty(Dirts, Childs, Obstacles, N, M)),
	bfs([[(X, Y)]], N, M, Obstacles, Childs, Path),
	get_direction(Path, Move),
	!.
