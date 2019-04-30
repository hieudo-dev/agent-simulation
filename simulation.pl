:-consult(robot_agent_2).

:-dynamic   dirt/2, 
            robot/3, 
            child/3, 
            obst/2, 
            crib/2, 
            board/2, 
            carrychild/1, 
            worldSize/2, 
            wins/1, 
            losses/1, 
            dirPer/2,
            dirMax/1.


%=======================================================================>
%                       Auxiliar Methods
%=======================================================================>


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
validPosition((X, Y)):- listBoard(L), listDirt(D), obst(X, Y),
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

paintRobot(X, Y):- robot(X, Y, 1), not(crib(X, Y)),write("R"), tab(1).
paintRobotInCrib(X, Y):- robot(X, Y, 1), crib(X, Y), write("RC"), tab(1).
paintSaveChild(X, Y):- child(X, Y, _), crib(X, Y), write("~"), tab(1).
paintDirtChild(X, Y):- dirt(X, Y), child(X, Y ,_), write("&"), tab(1).
paintChild(X, Y):- child(X, Y, _), not(dirt(X, Y)), not(crib(X, Y)), write("C"), tab(1).
paintCrib(X, Y):- crib(X, Y), write("@"), tab(1).
paintObst(X, Y):- obst(X, Y), write("O"), tab(1).
paintDirt(X, Y):- dirt(X, Y), write("#"), tab(1).
paintBoard(X, Y):- board(X, Y), free(X, Y), write(.), tab(1).

% sort_tuples(L, S):- sort(2,  @=<, L,  S).


%************************************************************************
%                    ENVIROMENT GENERATION
%************************************************************************

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

% --------------------------------------------------------------------------------------------------------
%    Wold Generation
% --------------------------------------------------------------------------------------------------------

generate_world(X, Y, Cr, Obs, Dir):-
   assert(worldSize(X, Y)),
   assert(dirMax(0)),
   initial_grid(X, Y, Y),
   generate_Crib(Cr),
   generate_obst(Obs),
   generate_dirt(Dir),
   generate_child(Cr),
   generate_robot().

% --------------------------------------------------------------------------------------------------------
%    Wold Generation
% --------------------------------------------------------------------------------------------------------

reset_world():-
   retractall(crib(_, _)),
   retractall(dirt(_, _)),
   retractall(board(_, _)),
   retractall(obst(_, _)),
   retractall(carrychild(_)),
   retractall(robot(_, _, _)),
   retractall(child(_, _, _)),
   retract(dirMax(_)),
   retract(worldSize(_, _)).

% --------------------------------------------------------------------------------------------------------
%    World Printing
% --------------------------------------------------------------------------------------------------------

paintWorld() :- worldSize(X, Y), Xs is X + 1, Ys is Y + 1,render_board(Xs, Ys, 0, 0).
paintHeader() :- worldSize(X, Y), write_list(["Tablero Inicial de ",X,"x",Y]).

% Render Board
render_board(X, _, X, _):- nl, !.
render_board(X, Y, X1, Y):- !, Z is X1 + 1, nl, render_board(X, Y, Z, 0).
render_board(X, Y, X1, Y1):- paintPos(X1, Y1), Z is Y1 + 1, render_board(X, Y, X1, Z), !.

% Get Elemnts in the N-th row
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


%************************************************************************
%                                SIMULATOR
%************************************************************************


%=======================================================================>
%        Child Pooping
%=======================================================================>

get_random_position((X, Y), (NX, NY)):- 
   findall((U, V), adjacent(X, Y, U, V), Adjacents),
   random_permutation(Adjacents, RandomAdjacents),
   RandomAdjacents = [(NX, NY)|_].

near_childs((X, Y), NearChilds):-
   findall((U, V),
   (adjacent(X, Y, U, V), child(U, V, _)),
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
   validPosition((NX, NY)),
   not(robot(NX, NY, _)),
   not(child(NX, NY, _)),
   retract(child(Xc, Yc, Id)),
   assert(child(NX, NY, Id)).
move_child(Id, (Xc, Yc), (NX, NY)):-
   obst(NX, NY),
   board(Xc, Yc),
   move_obst((Xc, Yc), (NX, NY)),
   retract(child(Xc, Yc, Id)),
   retract(obst(NX,NY)),
   assert(child(NX, NY, Id)),
   assert(board(NX, NY)).
move_child(Id, (Xc, Yc), (NX, NY)):-
   obst(NX, NY),
   dirt(Xc, Yc),
   move_obst((Xc, Yc), (NX, NY)),
   retract(child(Xc, Yc, Id)),
   retract(obst(NX,NY)),
   assert(child(NX, NY, Id)),
   assert(dirt(NX, NY)).
move_child(_,_,_).


move_obst((_, _), (Xa, Ya)):- 
   board(Xa, Ya),
   not(child(Xa,Ya,_)),
   not(robot(Xa,Ya,_)),
   retract(board(Xa, Ya)), 
   assert(obst(Xa, Ya)), !.
move_obst((_, _), (Xa, Ya)):- 
   dirt(Xa, Ya),
   not(child(Xa,Ya,_)),
   not(robot(Xa,Ya,_)),
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

carrying(true):- carrychild(_).
carrying(false):- not(carrychild(_)).


move_robot((X, Y), N, M):-
   carrying(Carrying),
   listDirt(Dirts),
   listObst(Obstacles),
   listCrib(Corrals),
   findall((A, B), child(A, B, _), Childs),
   % write([X, Y, N, M]), nl,
   % write(Carrying), nl,
   % write(Dirts), nl,
   % write(Obstacles), nl,
   % write(Childs), nl,
   % write(Corrals), nl,
   N1 is N+1, M1 is M+1,
   next_move((X, Y), N1, M1, Carrying, Dirts, Childs, Obstacles, Corrals, Moves),
   % write(Moves),nl,
   make_moves((X, Y), Moves).

make_moves(_, []).
make_moves(Position, [Move|T]):-
   make_move(Position, Move),
   robot(X, Y, _),
   make_moves((X, Y), T).


make_move((X, Y), clean):-
   dirt(X, Y),
   retract(dirt(X, Y)),
   assert(board(X, Y)).
make_move((X, Y), drop):-
   carrychild(Id),
   retract(carrychild(Id)),
   assert(child(X, Y, Id)).

make_move((X, Y), Move):-
   get_direction([(X, Y), (U, V)], Move),
   not(obst(X, Y)),
   retract(robot(X, Y, Id)),
   assert(robot(U, V, Id)),
   pickup_child((U, V)).

pickup_child((X, Y)):- 
   child(X, Y, Id),
   not(carrychild(_)),
   not(crib(X, Y)),
   retract(child(X, Y, Id)),
   assert(carrychild(Id)).
pickup_child(_).

%====================================================================================
%      SIMULATOR
%====================================================================================

simulator(N, M, ChildsCount, DirtPercent, ObstaclePercent, ChangeInterval):-
   assert(wins(0)),
   assert(losses(0)),
   assert(dirPer(0, 0)),
   ObstaclesCount is round(N * M * (ObstaclePercent / 100)),
   DirtCount is round(N * M * (DirtPercent / 100)),
   interval_wrapper(10, ChangeInterval, N, M, ChildsCount, ObstaclesCount, DirtCount),
   !.

interval_wrapper(0, _, _, _, _, _, _):- !.
interval_wrapper(T, Interval, N, M, ChildsCount, ObstaclesCount, DirtCount):-
   assert(worldSize(N, M)),
   generate_world(N, M, ChildsCount, ObstaclesCount, DirtCount),
   write(T), write(" - Generated World !"),nl,
   simulate(Interval, _), !,
   % paintWorld(),
   reset_world(),
   T1 is T-1,
   interval_wrapper(T1, Interval, N, M, ChildsCount, ObstaclesCount, DirtCount), !.

simulate(0, finished):- write("Simulation Finished"),    
                        recordDirtStat(), nl, !.
simulate(_, finished):-
   findall((X, Y), child(X, Y, _), Childs),
   sort(Childs, Ch),
   listCrib(Cribs), sort(Cribs, Cr),
   Cr == Ch,
   listDirt(Dirts), length(Dirts, Len),
   Len == 0,
   write("Robot finished job succesfully !"), nl,
   recordDirtStat(),
   updateWins(),
   !.
simulate(0, fired):-
   findall((X, Y), child(X, Y, _), Childs),
   sort(Childs, Ch),
   listCrib(Cribs), sort(Cribs, Cr),
   Cr == Ch,
   listDirt(Dirts), length(Dirts, Len),
   Len > 0,
   write("Robot finished job succesfully, But Couldn't Clean all the Board"), nl,
   recordDirtStat(),
   updateLosses(),
   !.
simulate(_, fired):- 
   worldSize(X, Y), X1 is X+1, Y1 is Y+1,
   listDirt(L),
   length(L, Dirts),
   round(X1 * Y1 * (60 / 100)) =< Dirts,
   write("El robot ha sido despedido !"), nl,
   recordDirtStat(),
   updateLosses(),
   !.
simulate(T, Outcome):- 
   % write_list(["Ronda: ", T]),
   turn_handler(), 
   Ts is T - 1, 
   % write("================================>"), nl,
   !, simulate(Ts, Outcome).

%=============================================================>
%  Turn Handler
%=============================================================>

turn_handler():-
   childs_turn(),
   updateMax(),
   robot_turn().
   % paintWorld().
   % sleep(1).

%=============================================================>
%  Robot Turn
%=============================================================>

%  Robot Turn
robot_turn():-
   worldSize(N, M),
   robot(X, Y, _),
   move_robot((X, Y), N, M).

%=============================================================>
%  Child Turn
%=============================================================>

childs_turn():-
   findall((Id, X, Y), (child(X, Y, Id), not(crib(X, Y))), List),
   sort(List, SortedChilds),
   maplist(child_turn, SortedChilds).

child_turn(Child):-
   Child = (Id, X, Y),
   % Create some dirt
   pooping_time((X, Y)),
   % Try to move
   get_random_position((X, Y), (NX, NY)),
   move_child(Id, (X, Y), (NX, NY)).


%=============================================================>
%  Write to file
%=============================================================>

writeStatsToFile(Mean, Wins, Losses):- 
   open('output.txt', write, Stream),
   write(Stream, "Dirt Mean: "), write(Stream, Mean),
   nl(Stream),
   write(Stream, "Total Wins: "), write(Stream, Wins),
   nl(Stream),
   write(Stream, "Total Losses: "), write(Stream, Losses),
   nl(Stream),
   close(Stream).

%=============================================================>
%  Statistics Extraction
%=============================================================>

updateWins():- 
   wins(X), 
   X1 is X + 1, 
   retract(wins(X)), 
   assert(wins(X1)).

updateLosses():-  
   losses(X), 
   X1 is X + 1, 
   retract(losses(X)), 
   assert(losses(X1)).

updateMax():-  
   countDirt(Cnt),
   dirMax(X),
   X < Cnt,
   retract(dirMax(_)),
   assert(dirMax(Cnt)).
updateMax().

countDirt(Cnt):-  
   listDirt(L),
   length(L, Cnt).

recordDirtStat():-   
   dirPer(Mean, N),
   dirMax(Cnt),
   NewMean is Mean + Cnt,
   N1 is N + 1,
   retract(dirPer(_, _)),
   assert(dirPer(NewMean, N1)).

extract_data():- 
   wins(X), 
   losses(Y),
   dirPer(Mean, N),
   M is Mean/N, 
   writeStatsToFile(M, X, Y).