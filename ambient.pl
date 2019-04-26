% worldSize(10, 10).
% dirtPorcent(8).
% dirtObst(5).
% dirtChild(3).
% dirtCorr(3).
% timeT(15).


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
paintPos(X, Y) :- paintChild(X,Y), !.
paintPos(X, Y) :- paintObst(X,Y), !.
paintPos(X, Y) :- paintDirt(X, Y), !.
paintPos(X, Y) :- paintCrib(X, Y), !.
paintPos(X, Y) :- paintBoard(X, Y), !.

paintSaveChild(X, Y):- child(X, Y, _), crib(X, Y), write("~"), tab(1).
paintDirtChild(X, Y):- dirt(X, Y), child(X, Y ,_), write("&"), tab(1).
paintChild(X, Y):- child(X, Y, _), not(dirt(X, Y)), not(crib(X, Y)), write("C"), tab(1).
paintCrib(X, Y):- crib(X, Y), write("@"), tab(1).
paintRobot(X, Y):- robot(X, Y, 1), write("R"), tab(1).
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
generate_world(X, Y, Cr, Obs, Dir):-
   initial_grid(X, Y, Y),
   generate_Crib(Cr),
   generate_obst(Obs),
   generate_dirt(Dir),
   generate_child(Cr),
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

get_top((X, Y), T):- TX is X - 1, TYa is Y + 1, TYb is Y - 1,
                        get_TopBot(TX, Y, TYa, TYb, T).

get_bot((X, Y), B):- TX is X + 1, TYa is Y + 1, TYb is Y - 1,
                        get_TopBot(TX, Y, TYa, TYb, B).

get_sides((X, Y), S):- Ly is Y - 1, Ry is Y + 1,
                        get_LeftRight(X, Ly, Ry, S).

get_topCount((X, Y), Tp):-TX is X - 1, TYa is Y + 1, TYb is Y - 1,
                    get_TopBotCount(TX, Y, TYa, TYb, Tp).

get_botCount((X, Y), Bt):- TX is X + 1, TYa is Y + 1, TYb is Y - 1,
                    get_TopBotCount(TX, Y, TYa, TYb, Bt).

get_sidesCount((X, Y), Sd):-Ly is Y - 1, Ry is Y + 1,
                    get_LeftRightCount(X, Ly, Ry, Sd).

get_topPop((X, Y), T):- TX is X - 1, TYa is Y + 1, TYb is Y - 1,
                get_TopBotPop(TX, Y, TYa, TYb, T).

get_botPop((X, Y), B):- TX is X + 1, TYa is Y + 1, TYb is Y - 1,
                get_TopBotPop(TX, Y, TYa, TYb, B).

get_sidesPop((X, Y), S):- Ly is Y - 1, Ry is Y + 1,
                get_LeftRightPop(X, Ly, Ry, S).


get_LeftRight(X, Ly, Ry, S):-   findall((X, Ly), board(X, Ly), FreeSpaces),
                                findall((X, Ly), dirt(X, Ly), DirtSpaces),
                                findall((X, Ly), obst(X, Ly), ObstSpaces),
                                findall((X, Ry), board(X,Ry), FreeSpacesa),
                                findall((X, Ry), dirt(X, Ry), DirtSpacesa),
                                findall((X, Ry), obst(X, Ry), ObstSpacesa),
                                append(FreeSpaces, FreeSpacesa, FS),
                                append(DirtSpaces, DirtSpacesa, DS),
                                append(ObstSpaces, ObstSpacesa, OS),
                                append(FS, DS, R), append(R, OS, S).


get_TopBot(TX, Y, TYa, TYb, T):-findall((TX, Y), board(TX, Y), FreeSpaces),
                                findall((TX, Y), dirt(TX, Y), DirtSpaces),
                                findall((TX, Y), obst(TX, Y), ObstSpaces),
                                findall((TX, TYa), board(TX, TYa), FreeSpacesa),
                                findall((TX, TYa), dirt(TX, TYa), DirtSpacesa),
                                findall((TX, TYa), obst(TX, TYa), ObstSpacesa),
                                findall((TX, TYb), board(TX, TYb), FreeSpacesb),
                                findall((TX, TYb), dirt(TX, TYb), DirtSpacesb),
                                findall((TX, TYb), obst(TX, TYb), ObstSpacesb),
                                append(FreeSpaces, FreeSpacesa, Fs),
                                append(Fs, FreeSpacesb, FS),
                                append(DirtSpaces, DirtSpacesa, Ds),
                                append(Ds, DirtSpacesb, DS),
                                append(ObstSpaces, ObstSpacesa, Os),
                                append(Os, ObstSpacesb, OS),
                                append(FS, DS, L),append(OS, L, T).

get_TopBotCount(TX, Y, TYa, TYb, Tp):-  findall((TX, Y), child(TX, Y, _), ChildSpaces),
                                        findall((TX, TYa), child(TX, TYa, _), ChildSpacsa),
                                        findall((TX, TYb), child(TX, TYb, _), Childpacesb),
                                        append(ChildSpaces, ChildSpacsa, T),
                                        append(T, Childpacesb, Ts),
                                        length(Ts, Tp).

get_LeftRightCount(X, Ly, Ry, Sp):-  findall((X, Ly), child(X, Ly, _), ChildSpaces),
                                    findall((X, Ry), child(X, Ry, _), ChildSpacsa),
                                    append(ChildSpaces, ChildSpacsa, S),
                                    length(S, Sp).

get_TopBotPop(TX, Y, TYa, TYb, T):-findall((TX, Y), board(TX, Y), FreeSpaces),
                                    findall((TX, Y), child(TX, Y, _), ChildSpaces),
                                    findall((TX, TYa), board(TX, TYa), FreeSpacesa),
                                    findall((TX, TYa), child(TX, TYa, _), ChildSpacesa),
                                    findall((TX, TYb), board(TX, TYb), FreeSpacesb),
                                    findall((TX, TYb), child(TX, TYb, _), ChildSpacesb),
                                    append(FreeSpaces, FreeSpacesa, Fs),
                                    append(Fs, FreeSpacesb, FS),
                                    append(ChildSpaces, ChildSpacesa, Os),
                                    append(Os, ChildSpacesb, OS),
                                    append(FS, OS, T).

get_LeftRightPop(X, Ly, Ry, S):- findall((X, Ly), board(X, Ly), FreeSpaces),
                                    findall((X, Ly), child(X, Ly, _), ChildSpaces),
                                    findall((X, Ry), board(X,Ry), FreeSpacesa),
                                    findall((X, Ry), child(X, Ry, _), ChildSpacesa),
                                    append(FreeSpaces, FreeSpacesa, FS),
                                    append(ChildSpaces, ChildSpacesa, OS),
                                    append(FS, OS, S).

child_count((X, Y), C):- get_topCount((X, Y), Tp), get_botCount((X, Y), Bt), get_sidesCount((X, Y), Sd), C is Tp + Bt + Sd.

get_random_position((X, Y), (NX, NY)):- get_top((X, Y), T), get_bot((X, Y), B), get_sides((X, Y), S),
                                        append(T, B, TB), append(TB, S, Adj), get_random(Adj, Pos),
                                        arg(1, Pos, NX), arg(2, Pos, NY).

get_valid_pop_positions((X, Y), L):- get_topPop((X, Y), Tp), get_botPop((X, Y), Bp), get_sidesPop((X, Y), Sp),
                                        append(Tp, Bp, R), append(R, Sp, L).

move_child(Id, (Xc, Yc), (NX, NY)):- validPosition((NX, NY)), !,
                                        get_valid_pop_positions((Xc, Yc), Posib), length(Posib, C),
                                        child_count((Xc, Yc), ChCnt),
                                        handle_poping(ChCnt, C, Posib, T), child_pop(T),retract(child(Xc, Yc, Id)),
                                        assert(board(Xc, Yc)), assert(child(NX, NY, Id)).
move_child(Id, (Xc, Yc), (NX, NY)):- move_obst((Xc, Yc), (NX, NY)), !,
                                        get_valid_pop_positions((Xc, Yc), Posib), length(Posib, C),
                                        child_count((Xc, Yc), ChCnt),
                                        handle_poping(ChCnt, C, Posib, T), child_pop(T),
                                        retract(child(Xc, Yc, Id)),
                                        assert(board(Xc, Yc)), retract(obst(NX,NY)),
                                        assert(child(NX, NY, Id)).

handle_poping(0, _, _, T):- T = [].
handle_poping(1, Cnt, L, T):- length(L, Cnt), random_between(1, Cnt, I),
                            nth1(I, L, Elem), append([Elem], [], T), !.
handle_poping(2, Cnt, L, T):- length(L, Cnt), get_twoRandoms(Cnt, A, B),
                            nth1(A, L, Elem), nth1(B, L, Elem2),
                            append([Elem, Elem2], [], T), !.
handle_poping(C, Cnt, L, T):- C >= 3, length(L, Cnt), Cnt < 6, T is L, !.
handle_poping(C, Cnt, L, T):- C >= 3, length(L, Cnt), get_sixRandoms(Cnt, A, B, C, D, E, F),
                            nth1(A, L, Elem), nth1(B, L, Elem2), nth1(C, L, Elem3),
                            nth1(D, L, Elem4), nth1(E, L, Elem5), nth1(F, L, Elem6),
                            append([Elem, Elem2, Elem3, Elem4, Elem5, Elem6], [], T), !.





move_obst((_, _), (Xa, Ya)):- listBoard(L), member((Xa, Ya), L), retract(board(Xa, Ya)), assert(obst(Xa, Ya)), !.
move_obst((X, Y), (Xa, Ya)):- Dx is Xa - X, Dy is Ya - Y,
                              Nx is Xa + Dx, Ny is Ya + Dy,
                              listObst(L), not(member((Nx, Ny), L)), move_obst((Xa, Ya), (Nx, Ny)).

child_pop([]):- nl.
child_pop([T|Ts]):- arg(1, T, X), arg(2, T, Y), listBoard(L), member((X, Y), L),
                    retract(board(X, Y)), assert(dirt(X, Y)), child_pop(Ts).
child_pop([T|Ts]):- arg(1, T, X), arg(2, T, Y), listBoard(L), not(member((X, Y), L)),
                    assert(dirt(X, Y)), child_pop(Ts).
 


%====================================================================================
%      SIMULATION
%====================================================================================

simulator(N, M, ChildsCount, DirtPercent, ObstaclePercent, ChangeInterval):-
   assert(worldSize(N, M)),
   ObstaclesCount is round(N * M * (ObstaclePercent / 100)),
   DirtCount is round(N * M * (DirtPercent / 100)),
   generate_world(N, M, ChildsCount, ObstaclesCount, DirtCount).
   %T is ChangeInterval*100,
   %N is ChildsCount+1,
   % simulation(T, N).

handler(1):- next_turn(1), !.
handler(N):- N >= 0, next_turn(N), N1 is N - 1, handler(N1).

next_turn(1):-nl, paintWorld(), !. % Handle Robot Movement.
next_turn(N):- findall((X, Y, N), child(X, Y, N), D), nth1(1, D, Ch),
                arg(1, Ch, X), arg(2, Ch, Ch2), arg(1, Ch2, Y), arg(2, Ch2, Id),
                get_random_position((X, Y), (Nx, Ny)), move_child(Id, (X, Y), (Nx, Ny)), !, paintWorld().

simulate(1, _):- write("Done Simulation"), paintWorld(), !.
simulate(T, N):- handler(N), Ts is T - 1, simulate(Ts, N).

%      simulator(8, 8, 0, 0, 0, 0).