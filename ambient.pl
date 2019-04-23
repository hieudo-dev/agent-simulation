% world(10, 10).
% dirtPorcent(8).
% dirtObst(5).
% dirtChild(3).
% dirtCorr(3).
% timeT(15).


:-dynamic dirt/2, robot/3, child/3, obst/2, crib/2, board/2.

% board(1,1).
% obst(2,1).
% child(3, 1, 2).



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

writeChar((X, Y)):- listBoard(L), member((X, Y), L), write(.), tab(1), !.
writeChar((X, Y)):- listObst(O), member((X, Y), O), write("O"), tab(1), !.
writeChar((X, Y)):- listDirt(D), member((X, Y), D), write("D"), tab(1), !.
writeChar((X, Y)):- listCrib(C), member((X, Y), C), write("C"), tab(1), !.
writeChar((X, Y, Id)):- listChild(Ch), member((X, Y, Id), Ch), write("Ch"), tab(1), !.




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

generate_robot:- listBoard(L), get_random(L, Pa), arg(1, Pa, X), arg(2, Pa, Y), retract(board(X,Y)), 
                 assert(robot(X, Y, 1)), !.


% Render Board
render_board(L, 1):- nth1(1, L, Elem), writeChar(Elem), nl, !.
render_board(L, P):- nth1(P, L, Elem), writeChar(Elem), Ps is P - 1, render_board(L, Ps).

% Get Elemnts in the N-esima row
get_boards(N, L):-findall((N, Y), board(N, Y), L).
get_obsts(N, L):-findall((N, Y), obst(N, Y), L).
get_dirts(N, L):-findall((N, Y), dirt(N, Y), L).
get_cribs(N, L):-findall((N, Y), crib(N, Y), L).
get_childs(N, T, L):-findall((N, Y, T), child(N, Y, T), L).
get_robot(N, P):-findall((N, Y, 1), robot(N, Y, 1), P).


% --------------------------------------------------------------------------------------------------------
%   Simulation
%   
%   move_child(Ch, Id, D) ::= params: Ch:child; Id:child.id; D:direction to move
%   move_robot(R, D) ::= params: R:robot; D:direction to move
%   
%   
%   
%   
% --------------------------------------------------------------------------------------------------------

move_child(Id, (Xc, Yc), (NX, NY)):- validPosition((NX, NY)), retract(child(Xc, Yc, Id)), 
                                    assert(board(Xc, Yc)), assert(child(NX, NY, Id)).
move_child(Id, (Xc, Yc), (NX, NY)):- move_obst((Xc, Yc), (NX, NY)), retract(child(Xc, Yc, Id)),
                                     assert(board(Xc, Yc)), retract(obst(2,1)), assert(child(NX, NY, Id)).


move_obst((_, _), (Xa, Ya)):- listBoard(L), member((Xa, Ya), L), retract(board(Xa, Ya)), assert(obst(Xa, Ya)), !.
move_obst((X, Y), (Xa, Ya)):- Dx is Xa - X, Dy is Ya - Y, 
                              Nx is Xa + Dx, Ny is Ya + Dy,
                              listObst(L), not(member((Nx, Ny), L)), move_obst((Xa, Ya), (Nx, Ny)). 