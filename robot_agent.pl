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

should_drop(Corrals, Position, [drop]):-member(Position, Corrals).
should_drop(Corrals, Position, []):-not(member(Position, Corrals)).

%		Carrying a child, enviroment is very dirty: Drop child
next_move(_, N, M, Carrying, Dirts, Childs, Obstacles, _, [drop]):-
	Carrying = true,
	is_very_dirty(Dirts, Childs, Obstacles, N, M),
	!.

% 		Not carrying a child, enviroment is very dirty, found dirt: Clean the dirt
next_move((X, Y), N, M, Carrying, Dirts, Childs, Obstacles, _, [clean]):-
	Carrying = false,
	is_very_dirty(Dirts, Childs, Obstacles, N, M),
	member((X, Y), Dirts),
	!.

%		No more childs left to move: Find dirt to clean
next_move((X, Y), N, M, Carrying, Dirts, Childs, Obstacles, Corrals, Move):-
	Carrying = false,
	sort(Childs, A),
	sort(Corrals, B),
	A == B,
	bfs([[(X, Y)]], N, M, Obstacles, Dirts, Path),
	get_direction(Path, Move),
	!.

	
% 		Not carrying a child, enviroment is very dirty: Find dirt to clean
next_move((X, Y), N, M, Carrying, Dirts, Childs, Obstacles, _, [Move]):-
	Carrying = false,
	is_very_dirty(Dirts, Childs, Obstacles, N, M),
	append(Childs, Obstacles, Obstacles1),
	bfs([[(X, Y)]], N, M, Obstacles1, Dirts, Path),
	get_direction(Path, Move),
	!.

%		Carrying a child, enviroment isnt very dirty: Move towards closest corral to drop child (1 Step)
next_move((X, Y), N, M, Carrying, Dirts, Childs, Obstacles, Corrals, [Move, drop]):-
	Carrying = true,
	not(is_very_dirty(Dirts, Childs, Obstacles, N, M)),
	append(Obstacles, Childs, RobotObstacles),
	subtract(RobotObstacles, Corrals, Obstacles1),
	subtract(Corrals, Childs, EmptyCorrals),
	bfs([[(X, Y)]], N, M, Obstacles1, EmptyCorrals, [S, T|PathTail]),
	member(T, Corrals),
	get_direction([S, T|PathTail], Move),
	!.

%		Carrying a child, enviroment isnt very dirty: Move towards closest corral to drop child (2 Step)
next_move((X, Y), N, M, Carrying, Dirts, Childs, Obstacles, Corrals, [Move1, Move2| Drop]):-
	Carrying = true,
	not(is_very_dirty(Dirts, Childs, Obstacles, N, M)),
	append(Obstacles, Childs, RobotObstacles),
	subtract(RobotObstacles, Corrals, Obstacles1),
	subtract(Corrals, Childs, EmptyCorrals),
	bfs([[(X, Y)]], N, M, Obstacles1, EmptyCorrals, [S, T, R|PathTail]),
	get_direction([S, T, R|PathTail], Move1),
	get_direction([T, R|PathTail], Move2),
	should_drop(Corrals, R, Drop),
	!.

% 		Not carrying a child, enviroment isn't very dirty: Move towards closest child to carry
next_move((X, Y), N, M, Carrying, Dirts, Childs, Obstacles, Corrals, [Move]):-
	Carrying = false,
	not(is_very_dirty(Dirts, Childs, Obstacles, N, M)),
	subtract(Childs, Corrals, FreeChilds),
	bfs([[(X, Y)]], N, M, Obstacles, FreeChilds, Path),
	get_direction(Path, Move),
	!.