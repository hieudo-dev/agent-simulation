worldSize(10, 10).
% dirtPorcent(8).
% dirtObst(5).
% dirtChild(3).
% dirtCorr(3).
% timeT(15).


:-consult(robot_agent).

:-dynamic dirt/2, robot/3, child/3, obst/2, crib/2, board/2, carrychild/3, savechild/3, worldSize/2.

% Auxiliar Methods
% get_random ::= return: C a random position of the list L.
get_random(L, C):- length(L, N), random_between(1, N, R), nth1(R, L, C).

get_twoRandoms(N, A, B):-random_between(1, N, A), random_between(1, N, B).

get_sixRandoms(N, A, B, C, D, E, F):-random_between(1, N, A), random_between(1, N, B),
                                        random_between(1, N, C), random_between(1, N, D),
                                        random_between(1, N, E), random_between(1, N, F).



% adjacent ::= params: P => Reference point to find its adjacent points
%          ::= return: (NX, NY) => Adjacent point of P in 4 directions (up, down, right, left) in that order.
adjacent(P, (NX, NY)):- arg(1, P, X), arg(2, P, Y), NX is X, NY is Y - 1, board(X, NY).
adjacent(P, (NX ,NY)):- arg(1, P, X), arg(2, P, Y), NX is X, NY is Y + 1, board(X, NY).
adjacent(P, (NX, NY)):- arg(1, P, X), arg(2, P, Y), NX is X + 1, NY is Y, board(NX, Y).
adjacent(P, (NX, NY)):- arg(1, P, X), arg(2, P, Y), NX is X - 1, NY is Y, board(NX, Y).

% checkMember ::= Verify what type the point is
checkMember((X, Y), B):-member((X, Y), B).
checkMemberWithId((X, Y, Id), B):- member((X, Y, Id), B).

% validPosition(X,Y)
validPosition((X, Y)):- listBoard(L), listDirt(D),
                        append(L, D, R), member((X,Y), R), !.

write_list([X|Xs]) :-  write(X), write_list(Xs).
write_list([]) :- nl.

free(X, Y):- board(X, Y), not(child(X, Y, _)), not(dirt(X, Y)), not(obst(X, Y)), not(crib(X, Y)), not(robot(X, Y, 1)).

paintPos(X, Y) :- paintSaveChild(X, Y), !.
paintPos(X, Y) :- paintDirtChild(X, Y), !.
paintPos(X, Y) :- paintRobot(X, Y), !.
paintPos(X, Y) :- paintRobotInCrib(X, Y), !.
paintPos(X, Y) :- paintChild(X,Y), !.
paintPos(X, Y) :- paintObst(X,Y), !.
paintPos(X, Y) :- paintDirt(X, Y), !.
paintPos(X, Y) :- paintCrib(X, Y), !.
paintPos(X, Y) :- paintBoard(X, Y), !.

paintSaveChild(X, Y):- child(X, Y, _), crib(X, Y), write("~"), tab(1).
paintDirtChild(X, Y):- dirt(X, Y), child(X, Y ,_), write("&"), tab(1).
paintChild(X, Y):- child(X, Y, _), not(dirt(X, Y)), not(crib(X, Y)), write("C"), tab(1).
paintCrib(X, Y):- crib(X, Y), write("@"), tab(1).
paintRobot(X, Y):- robot(X, Y, 1), not(crib(X, Y)),write("R"), tab(1).
paintRobotInCrib(X, Y):- robot(X, Y, 1), crib(X, Y), write("RC"), tab(1).
paintObst(X, Y):- obst(X, Y), write("O"), tab(1).
paintDirt(X, Y):- dirt(X, Y), write("#"), tab(1).
paintBoard(X, Y):- board(X, Y), free(X, Y), write(.), tab(1).

% sort_tuples(L, S):- sort(2,  @=<, L,  S).





% --------------------------------------------------------------------------------------------------------
%   Initial Board
%
%   Create all the facts of board, and add them to the enviorment in execution time.
%
%   Ej: initial_grid(2,2,2).
%
%   lisiting. ::= result:
%               ...
%               board(2,2)
%               board(2,1)
%               board(1,2)
%               board(1,1)
%               ...
%
%   listBoard(B), listDirt(D), listCrib(C), listObst(O), listChild(Ch) ::= return:
%                           ::= return: A list with each valid tuple (X,Y) for every type defined in the current environment.
%   Ej: listBoard(L). # After initial_grid(2,2,2).
%
%   listing. ::= result:
%            L = [(2,2), (2,1), (1,2), (1,1)].
% --------------------------------------------------------------------------------------------------------

initial_grid(0, 0, _):- assert(board(0, 0)), !.
initial_grid(X, 0, M):- XS is X - 1, Y is M, assert(board(X, 0)), initial_grid(XS, Y, M), !.
initial_grid(X, Y, M):- YS is Y - 1, assert(board(X, Y)), initial_grid(X, YS, M).

listBoard(B):-findall((X, Y), board(X, Y), B).
listDirt(D):-findall((X,Y), dirt(X,Y), D).
listCrib(C):-findall((X,Y), crib(X,Y), C).
listObst(O):-findall((X,Y), obst(X,Y), O).
listChild(Ch):-findall((X, Y, Id), child(X, Y, Id), Ch).

% --------------------------------------------------------------------------------------------------------
%   Crib Generation
% --------------------------------------------------------------------------------------------------------

generate_Crib(N):-listBoard(L), generate_crib(N, L), !.

generate_crib(1, Adj):- get_random(Adj, C), arg(1, C, X), arg(2, C, Y), retract(board(X, Y)),
                        assert(crib(X, Y)).
generate_crib(N, Adj):- get_random(Adj, C), arg(1, C, X), arg(2, C, Y), retract(board(X, Y)),
                        assert(crib(X, Y)), Ns is N - 1, get_crib_adj((X, Y), L),generate_crib(Ns, L).

% --------------------------------------------------------------------------------------------------------
%   Obst Generation
% --------------------------------------------------------------------------------------------------------

generate_obst(1):- listBoard(L), get_random(L, Pa), arg(1, Pa, X), arg(2, Pa, Y), retract(board(X,Y)),
                    assert(obst(X,Y)), !.

generate_obst(N):- listBoard(L), get_random(L, Pa), arg(1, Pa, X), arg(2, Pa, Y), retract(board(X,Y)),
                    assert(obst(X,Y)), NS is N - 1, generate_obst(NS).

% --------------------------------------------------------------------------------------------------------
%   Dirt Generation
% --------------------------------------------------------------------------------------------------------

generate_dirt(1):- listBoard(L), get_random(L, Pa), arg(1, Pa, X), arg(2, Pa, Y), retract(board(X,Y)),
                    assert(dirt(X,Y)), !.

generate_dirt(N):- listBoard(L), get_random(L, Pa), arg(1, Pa, X), arg(2, Pa, Y), retract(board(X,Y)),
                    assert(dirt(X,Y)), NS is N - 1, generate_dirt(NS).

% --------------------------------------------------------------------------------------------------------
%   Child Generation
% --------------------------------------------------------------------------------------------------------

generate_child(1):- listBoard(L), listDirt(D), append(L, D, R), get_random(R, Pa),
                    arg(1, Pa, X), arg(2, Pa, Y), member((X,Y), R), !,
                    assert(child(X, Y, 2)), !.

generate_child(N):- listBoard(L), listDirt(D), listChild(Ch), append(L, D, R), get_random(R, Pa),
                    arg(1, Pa, X), arg(2, Pa, Y), member((X,Y), R), not(member((X, Y), Ch)), !,
                    Id is N + 1, assert(child(X, Y, Id)), NS is N - 1, generate_child(NS).

% --------------------------------------------------------------------------------------------------------
%    Robot Generation
% --------------------------------------------------------------------------------------------------------

generate_robot() :- listBoard(L), get_random(L, Pa), arg(1, Pa, X), arg(2, Pa, Y),
                 assert(robot(X, Y, 1)), !.

% Paint Board
generate_world(X, Y, Cr, Obs, Dir):-
   initial_grid(X, Y, Y), write("Initial Grid"), nl,
   generate_Crib(Cr), write("Initial Crib"), nl,
   generate_obst(Obs), write("Initial Obst"), nl,
   generate_dirt(Dir), write("Initial Dir"), nl,
   generate_child(Cr), write("Initial Cr"), nl,
   generate_robot().
paintWorld() :- worldSize(X, Y), paintHeader(), Xs is X + 1, Ys is Y + 1,render_board(Xs, Ys, 0, 0).
paintHeader() :- worldSize(X, Y), write_list(["Tablero Inicial de ",X,"x",Y]).

% Render Board
render_board(X, _, X, _):- nl, !.
render_board(X, Y, X1, Y):- !, Z is X1 + 1, nl, render_board(X, Y, Z, 0).
render_board(X, Y, X1, Y1):- paintPos(X1, Y1), Z is Y1 + 1, render_board(X, Y, X1, Z), !.

% Get Elemnts in the N-esima row
get_boards(N, L):-findall((N, Y), board(N, Y), L).
get_obsts(N, L):-findall((N, Y), obst(N, Y), L).
get_dirts(N, L):-findall((N, Y), dirt(N, Y), L).
get_cribs(N, L):-findall((N, Y), crib(N, Y), L).
get_childs(N, T, L):-findall((N, Y, T), child(N, Y, T), L).
get_robot(N, P):-findall((N, Y, 1), robot(N, Y, 1), P).

get_row(N, L):- get_boards(N, B), get_obsts(N, O),
                append(B, O, R1), get_dirts(N, D),
                append(D, R1, R2), get_cribs(N, C),
                append(C, R2, R3), get_childs(N, _, Ch),
                append(Ch, R3, R4), get_robot(N, P),
                append(P, R4, L).

%  Gets (X-1, Y) & (X+1, Y).
get_topbot((_, Y), Xtop, Xbot, L):- findall((Xtop, Y), board(Xtop, Y), Ts),
                                    findall((Xbot, Y), board(Xbot, Y), Bs),
                                    append(Ts, Bs, L).
%  Gets (X, Y - 1) & (X, Y + 1).
get_sides((X, _), Yleft, Yright, L):- findall((X, Yleft), board(X, Yleft), Ls),
                                       findall((X, Yright), board(X, Yright), Rs),
                                       append(Ls, Rs, L).
%  Get adjacent in four directions of (X, Y)           
get_crib_adj((X, Y), Adj):- Xtop is X - 1, Xbot is X + 1,
                              Yleft is Y - 1, Yright is Y + 1,
                              get_topbot((X, Y), Xtop, Xbot, Tb),
                              get_sides((X, Y), Yleft, Yright, Sb),
                              append(Tb, Sb, Adj).



% --------------------------------------------------------------------------------------------------------
%   Simulation
%
%   move_child(...) ::= return: Move a Child with id:Id
%   move_obst(...) ::= return: Handles when a child pushes an obstacule
%   move_robot(...) ::= params: Handels Robot
%
%
%
%
% --------------------------------------------------------------------------------------------------------


%=======================================================================>
%        Child Pooping
%=======================================================================>

get_random_position((X, Y), (NX, NY)):- 
   findall((U, V), adjacent(X, Y, U, V), Adjacents),
   random_permutation(Adjacents, RandomAdjacents),
   RandomAdjacents = [(NX, NY)|_].

near_childs((X, Y), NearChilds):-
   findall((U, V),
   (adjacent(X, Y, U, V), child(U, V, _))),
   AdjChilds),
   append(AdjChilds, [(X, Y)], NearChilds).

poop_amount(NearChildsCnt, 1):- NearChildsCnt == 1.
poop_amount(NearChildsCnt, 2):- NearChildsCnt == 2.
poop_amount(NearChildsCnt, 6):- NearChildsCnt >= 3.


pooping_time((Xc,Yc)):-
   findall((X,Y), adjacent(Xc, Yc, X, Y), Adjacents),
   random_permutation(Adjacents, RandomAdjs),
   near_childs((Xc, Yc), NearChilds),
   length(NearChilds, L),
   poop_amount(L, PoopsAmt),
   child_poop(PoopsAmt, RandomAdjs).


child_poop(0, _).
child_poop(N, [(X, Y)|Ts]):- 
   board(X, Y),
   retract(board(X, Y)), 
   assert(dirt(X, Y)), 
   N1 is N-1,
   child_poop(N1, Ts).
child_poop(N, [(X, Y)|Ts]):- 
   (child(X, Y, _); robot(X, Y, _)), 
   assert(dirt(X, Y)), 
   N1 is N-1,
   child_poop(N1, Ts).
child_poop(N, [_|Ts]):-
   N1 is N-1,
   child_poop(N1, Ts).

%=======================================================================>
%        Child Movement
%=======================================================================>

move_child(Id, (Xc, Yc), (NX, NY)):-
   validPosition((NX, NY)), !,
   retract(child(Xc, Yc, Id)),
   assert(child(NX, NY, Id)).
move_child(Id, (Xc, Yc), (NX, NY)):-
   obst(NX, NY),
   board(Xc, Yc),
   move_obst((Xc, Yc), (NX, NY)), !,
   retract(child(Xc, Yc, Id)),
   retract(obst(NX,NY)),
   assert(child(NX, NY, Id)),
   assert(board(NX, NY)).
move_child(Id, (Xc, Yc), (NX, NY)):-
   obst(NX, NY),
   dirt(Xc, Yc),
   move_obst((Xc, Yc), (NX, NY)), !,
   retract(child(Xc, Yc, Id)),
   retract(obst(NX,NY)),
   assert(child(NX, NY, Id)),
   assert(dirt(NX, NY)).
move_child(_,_,_).


move_obst((_, _), (Xa, Ya)):- 
   board(Xa, Ya),
   retract(board(Xa, Ya)), 
   assert(obst(Xa, Ya)), !.
move_obst((_, _), (Xa, Ya)):- 
   dirt(Xa, Ya),
   retract(dirt(Xa, Ya)), 
   assert(obst(Xa, Ya)), !.
move_obst((X, Y), (Xa, Ya)):- 
   Dx is Xa - X, Dy is Ya - Y,
   Nx is Xa + Dx, Ny is Ya + Dy,
   obst(Xa, Ya), 
   move_obst((Xa, Ya), (Nx, Ny)).

%====================================================================================
%     Robot Behaviour
%====================================================================================

carrying(true):- carrychild(_, _, _).
carrying(false):- not(carrychild(_, _, _)).


move_robot((X, Y), N, M):-
   carrying(Carrying),
   listDirt(Dirts),
   listChild(Childs),
   listObst(Obstacles),
   listCrib(Corrals),
   next_move((X, Y), N, M, Carrying, Dirts, Childs, Obstacles, Corrals, Moves),
   make_moves((X, Y), Moves).

make_moves(Position, [Move|T]):-
   make_move(Position, Move),
   make_moves(Position, T).


make_move((X, Y), clean):-
   dirt(X, Y),
   retract(dirt(X, Y)).
make_move((X, Y), drop):-
   carrychild(A, B, Id),
   retract(carrychild(A, B, Id)),
   assert(child(X, Y, Id)).

make_move((X, Y), Move):-
   get_direction([(X, Y), (U, V)], Move),
   not(obst(X, Y)),
   retract(robot(X, Y)),
   assert(robot(U, V)),
   clean_up((X, Y)).

%====================================================================================
%      SIMULATION
%====================================================================================

simulator(N, M, ChildsCount, DirtPercent, ObstaclePercent, ChangeInterval):-
   assert(worldSize(N, M)),
   ObstaclesCount is round(N * M * (ObstaclePercent / 100)),
   DirtCount is round(N * M * (DirtPercent / 100)),
   generate_world(N, M, 4, 4, 4),
   write("Generated World !"),nl,
   X is ChildsCount+1,
   !, simulate(500, X).
   %T is ChangeInterval*100,
   %N is ChildsCount+1,
   % simulation(T, N).

   simulate(0, _):- !, write("Done Simulation").
   simulate(T, N):- turn_handler(), Ts is T - 1, write_list(["Ronda: ", T]),nl, simulate(Ts, N).

%  Turn Handler
turn_handler():-
   % childs_turn(),
   robot_turn(),
   %checkear si se ha llegado al 60% de suciedad y terminar
   paintWorld().
   sleep(1).


%  Robot Turn
robot_turn():-
   worldSize(N, M),
   robot(X, Y, _),
   move_robot((X, Y), N, M).

%  Child Turn
childs_turn():-
   findall((Id, X, Y), child(X, Y, Id), List),
   sort(List, SortedChilds),
   maplist(child_turn, SortedChilds).

child_turn(Child):-
   Child = (Id, X, Y),
   pooping_time((X, Y)),
   get_random_position((X, Y), (NX, NY)),
   move_child(Id, (X, Y), (NX, NY)).

%      simulator(8, 8, 0, 0, 0, 0).