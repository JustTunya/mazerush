:- use_module(library(lists)).

% make_enemies(+Count, +Maze, +N, +MinD, +Fixed, -Enemies)
make_enemies(Count, Maze, N, MinD, Fixed, Enemies) :-
    place_enemies(Count, Maze, N, MinD, Fixed, [], Enemies).

% place_enemies(+K, +Maze, +N, +MinD, +Fixed, +Acc, -Enemies)
place_enemies(0, _, _, _, _, Acc, Enemies) :- sort(Acc, Enemies).
place_enemies(K, Maze, N, MinD, Fixed, Acc, Enemies) :-
    K > 0,
    random_valid_cell(Maze, N, X, Y),
    far_enough((X,Y), Fixed, MinD),
    far_enough((X,Y), Acc,  MinD),
    \+ member((X,Y), Acc),
    K1 is K-1,
    place_enemies(K1, Maze, N, MinD, Fixed, [(X,Y)|Acc], Enemies).
place_enemies(K, Maze, N, MinD, Fixed, Acc, Enemies) :-
    place_enemies(K, Maze, N, MinD, Fixed, Acc, Enemies).

% move_enemies(+Maze, +N, +EnemiesList, +PlayerX, +PlayerY, -NewEnemiesList)
move_enemies(_, _, [], _, _, []).
move_enemies(Maze, N, [(Ex, Ey) | Rest], PlayerX, PlayerY, [(Nx, Ny) | NewRest]) :-
    move_enemy(Maze, N, Ex, Ey, PlayerX, PlayerY, Nx, Ny),
    move_enemies(Maze, N, Rest, PlayerX, PlayerY, NewRest).

% move_enemy(+Maze, +N, +EnemyX, +EnemyY, +PlayerX, +PlayerY, -NextX, -NextY)
move_enemy(_, _, EnemyX, EnemyY, PlayerX, PlayerY, NextX, NextY) :-
    EnemyX =:= PlayerX, EnemyY =:= PlayerY, !,
    NextX = EnemyX, NextY = EnemyY.
move_enemy(Maze, N, EnemyX, EnemyY, PlayerX, PlayerY, NextX, NextY) :-
    shortest_path(N, Maze, EnemyX, EnemyY, PlayerX, PlayerY, Path),
    next_enemy_step(Path, EnemyX, EnemyY, NextX, NextY).

% enemy_collision(+PlayerX, +PlayerY, +Enemies)
enemy_collision(PlayerX, PlayerY, Enemies) :-
    member((PlayerX,PlayerY), Enemies).

% next_enemy_step(+Path, +EnemyX, +EnemyY, -NextX, -NextY)
next_enemy_step([], EnemyX, EnemyY, EnemyX, EnemyY).
next_enemy_step([_], EnemyX, EnemyY, EnemyX, EnemyY).
next_enemy_step([_, (NextX, NextY)|_], _, _, NextX, NextY).

% !-- Breadth-First Search Algorithm --!

% ARGUMENTS:
% N - size of the maze
% Maze - the maze grid
% SX, SY - starting coordinates
% GX, GY - goal coordinates
% Path - shortest path as [(X,Y), ...]

% shortest_path(+N, +Maze, +SX, +SY, +GX, +GY, -Path)
shortest_path(N, Maze, SX, SY, GX, GY, Path) :-
    bfs([(SX,SY)], [(SX,SY)-none], N, Maze, (GX,GY), Parents),
    build_path((GX,GY), Parents, Rev),
    reverse(Rev, Path).

% Core BFS implementation
% bfs(+Queue, +Parents, +N, +Maze, +Goal, -FinalParents)
bfs([], _, _, _, _, _) :- fail.
bfs([Node|_], Parents, _, _, Goal, Parents) :- Node = Goal, !.
bfs([Node|Queue], Parents, N, Maze, Goal, FinalParents) :-
    findall(Next,
        next_move(Node, Next, N, Maze, Parents),
        NextMoves),
    maplist({Node}/[P, P-Node]>>true, NextMoves, Links),
    append(Queue, NextMoves, NewQueue),
    append(Parents, Links, NewParents),
    bfs(NewQueue, NewParents, N, Maze, Goal, FinalParents).

% Generate valid neighbors not yet visited
% next_move(+Current, -Next, +N, +Maze, +Parents)
next_move((X,Y), (NX,NY), N, Maze, Parents) :-
    member((DX,DY), [(1,0),(-1,0),(0,1),(0,-1)]),
    NX is X+DX, NY is Y+DY,
    valid_move(Maze, N, NX, NY),
    \+ memberchk((NX,NY)-_, Parents).

% Build the path from the Parents mapping
% build_path(+Position, +Parents, -Path)
build_path((X,Y), Parents, [(X,Y)|Rest]) :-
    memberchk((X,Y)-Parent, Parents),
    extend_path(Parent, Parents, Rest).

% Extend path recursively from parent nodes
% extend_path(+Parent, +Parents, -Rest)
extend_path(none, _, []) :- !.
extend_path(Parent, Parents, Rest) :-
    build_path(Parent, Parents, Rest).