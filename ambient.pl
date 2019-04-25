worldSize(10, 10).
% dirtPorcent(8).
% dirtObst(5).
% dirtChild(3).
% dirtCorr(3).
% timeT(15).


:-dynamic dirt/2, robot/3, child/3, obst/2, crib/2, board/2.

% Auxiliar Methods
% get_random ::= return: A random position of the list L.
get_random(L, C):- length(L, N), random_between(1, N, R), nth1(R, L, C).

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
paintPos(X, Y) :- paintChild(X,Y), !.
paintPos(X, Y) :- paintObst(X,Y), !.
paintPos(X, Y) :- paintDirt(X, Y), !.
paintPos(X, Y) :- paintCrib(X, Y), !.
paintPos(X, Y) :- paintBoard(X, Y), !.

paintSaveChild(X, Y):- child(X, Y, _), crib(X, Y), write("~"), tab(1).
paintDirtChild(X, Y):- dirt(X, Y), child(X, Y ,_), write("&"), tab(1).
paintBoard(X, Y):- board(X, Y), free(X, Y), write(.), tab(1).
paintCrib(X, Y):- crib(X, Y), write("@"), tab(1).
paintRobot(X, Y):- robot(X, Y, 1), write("R"), tab(1).
paintChild(X, Y):- child(X, Y, _), not(dirt(X, Y)), not(crib(X, Y)), write("C"), tab(1).
paintObst(X, Y):- obst(X, Y), write("O"), tab(1).
paintDirt(X, Y):- dirt(X, Y), write("#"), tab(1).
                    
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

initial_grid(1, 1, _):- assert(board(1, 1)), !.
initial_grid(X, 1, M):- XS is X - 1, Y is M, assert(board(X, 1)), initial_grid(XS, Y, M), !.
initial_grid(X, Y, M):- YS is Y - 1, assert(board(X, Y)), initial_grid(X, YS, M).

listBoard(B):-findall((X, Y), board(X, Y), B).
listDirt(D):-findall((X,Y), dirt(X,Y), D).
listCrib(C):-findall((X,Y), crib(X,Y), C).
listObst(O):-findall((X,Y), obst(X,Y), O).
listChild(Ch):-findall((X, Y, Id), child(X, Y, Id), Ch).

% --------------------------------------------------------------------------------------------------------
%   Crib Generation
% --------------------------------------------------------------------------------------------------------

generate_Crib(N):-listBoard(L), generate_crib(N, L).

generate_crib(1, LAdy):- get_random(LAdy, Pa), arg(1, Pa, X), arg(2, Pa, Y), retract(board(X, Y)), 
                         assert(crib(X, Y)), !.

generate_crib(N, LAdy):- get_random(LAdy, Pa), arg(1, Pa, X), arg(2, Pa, Y), retract(board(X, Y)), 
                         assert(crib(X, Y)), findall(D, adjacent(Pa, D), R), NX is N - 1, 
                         generate_crib(NX, R).

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
                    arg(1, Pa, X), arg(2, Pa, Y), member((X,Y), L), !, retract(board(X,Y)), 
                    assert(child(X, Y, 2)), !.

generate_child(1):- listBoard(L), listDirt(D), append(L, D, R), get_random(R, Pa), 
                    arg(1, Pa, X), arg(2, Pa, Y), member((X,Y), D), !, assert(child(X, Y, 2)), !.

generate_child(N):- listBoard(L), listDirt(D), listChild(Ch), append(L, D, R), get_random(R, Pa), 
                    arg(1, Pa, X), arg(2, Pa, Y), member((X,Y), L), not(member((X, Y), Ch)), !, retract(board(X,Y)), 
                    Id is N + 1, assert(child(X, Y, Id)), NS is N - 1, generate_child(NS).

generate_child(N):- listBoard(L), listDirt(D), listChild(Ch), append(L, D, R), get_random(R, Pa), 
                    arg(1, Pa, X), arg(2, Pa, Y), member((X,Y), D), not(member((X, Y), Ch)), !, 
                    Id is N + 1, assert(child(X, Y, Id)), NS is N - 1, generate_child(NS).

% --------------------------------------------------------------------------------------------------------
%    Robot Generation 
% --------------------------------------------------------------------------------------------------------

generate_robot() :- listBoard(L), get_random(L, Pa), arg(1, Pa, X), arg(2, Pa, Y), retract(board(X,Y)), 
                 assert(robot(X, Y, 1)), !.

% Paint Board
generate_world(X, Y):- initial_grid(X, Y, Y), generate_Crib(4),
generate_obst(6), generate_dirt(3), generate_child(4), generate_robot().
paintWorld(I) :- worldSize(X, Y), paintHeader(I), !, render_board(X,Y,1,1).
paintHeader(I) :- worldSize(X, Y), write("Iteration ",I), write_list(["Tablero Inicial de ",X,"x",Y]).

% Render Board
render_board(X, _, X, _):- nl, !.
render_board(X, Y, X1, Y):- !, Z is X1 + 1, nl, render_board(X, Y, Z, 1).
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

get_top((X, Y), L, T):-.
get_bot((X, Y), L, B):-.
get_sides((X, Y), B, F):-.
get_random_position((X, Y), F, (NX, NY)):-.

get_new_child_position((X, Y), (NX, NY)):-  get_top((X, Y), [], T), get_bot((X, Y), L, B), 
                                            get_sides((X, Y), B, F), get_random_position((X, Y), F, (NX, NY)).


move_child(Id, (Xc, Yc), (NX, NY)):- validPosition((NX, NY)), retract(child(Xc, Yc, Id)), 
                                    assert(board(Xc, Yc)), assert(child(NX, NY, Id)).
move_child(Id, (Xc, Yc), (NX, NY)):- move_obst((Xc, Yc), (NX, NY)), retract(child(Xc, Yc, Id)),
                                     assert(board(Xc, Yc)), retract(obst(2,1)), assert(child(NX, NY, Id)).


move_obst((_, _), (Xa, Ya)):- listBoard(L), member((Xa, Ya), L), retract(board(Xa, Ya)), assert(obst(Xa, Ya)), !.
move_obst((X, Y), (Xa, Ya)):- Dx is Xa - X, Dy is Ya - Y, 
                              Nx is Xa + Dx, Ny is Ya + Dy,
                              listObst(L), not(member((Nx, Ny), L)), move_obst((Xa, Ya), (Nx, Ny)).                       