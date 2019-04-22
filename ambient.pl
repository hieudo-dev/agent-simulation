% world(10, 10).
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
listChild(Ch):-findall((X,Y), child(X, Y, _), Ch).
get_robot(P):-findall((X,Y), robot(X, Y, 1), P).

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
                    arg(1, Pa, X), arg(2, Pa, Y), member((X,Y), L), retract(board(X,Y)), 
                    assert(child(X,Y)), !.

generate_child(1):- listBoard(L), listDirt(D), append(L, D, R), get_random(R, Pa), 
                    arg(1, Pa, X), arg(2, Pa, Y), member((X,Y), D), assert(child(X,Y)), !.

generate_child(N):- listBoard(L), listDirt(D), append(L, D, R), get_random(R, Pa), 
                    arg(1, Pa, X), arg(2, Pa, Y), member((X,Y), L), retract(board(X,Y)), 
                    assert(child(X,Y)), NS is N - 1, generate_child(NS).

generate_child(N):- listBoard(L), listDirt(D), append(L, D, R), get_random(R, Pa), 
                    arg(1, Pa, X), arg(2, Pa, Y), member((X,Y), D), 
                    assert(child(X,Y)), NS is N - 1, generate_child(NS).

% --------------------------------------------------------------------------------------------------------
%    Robot Generation 
% --------------------------------------------------------------------------------------------------------

generate_robot:- listBoard(L), get_random(L, Pa), arg(1, Pa, X), arg(2, Pa, Y), retract(board(X,Y)), 
                 assert(robot(X, Y, 1)), !.


% Render Board
% render_board(1, 1, B, D, C, O, CH):- .
% 

% paint(X,Y):- listBoard(B), checkMember((X, Y), B), write(.), tab(1).
% paint(X,Y):- listBoard(D), checkMember((X, Y), D), write(#), tab(1).
% paint(X,Y):- listBoard(O), checkMember((X, Y), O), write(O), tab(1).
% paint(X,Y):- listBoard(S), checkMember((X, Y), S), write(█), tab(1).
% paint(X, Y, Id):- listBoard(Ch), checkMemberWithId((X, Y, Id), Ch), write(.), tab(1).
% paintRobot:- write(®), tab(1).


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