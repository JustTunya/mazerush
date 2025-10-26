:- use_module(library(lists)).

move_enemy(Maze, N, EnemyX, EnemyY, PlayerX, PlayerY, NextX, NextY) :-
    (EnemyX =:= PlayerX, EnemyY =:= PlayerY ->
        NextX = EnemyX, NextY = EnemyY
    ;   shortest_path(N, Maze, EnemyX, EnemyY, PlayerX, PlayerY, Path),
        next_enemey_step(Path, EnemyX, EnemyY, NextX, NextY)
    ).

enemy_collision(PlayerX, PlayerY, EnemyX, EnemyY) :-
    PlayerX =:= EnemyX, PlayerY =:= EnemyY.

next_enemey_step([], EnemyX, EnemyY, EnemyX, EnemyY).
next_enemey_step([_], EnemyX, EnemyY, EnemyX, EnemyY).
next_enemey_step([_, (NextX, NextY)|_], _, _, NextX, NextY).

% !-- Breadth-First Search Algorithm --!

% ARGUMENTS:
% N - size of the maze
% Maze - the maze grid
% SX, SY - starting coordinates
% GX, GY - goal coordinates
% Path - shortest path as [(X,Y), ...]
shortest_path(N, Maze, SX, SY, GX, GY, Path) :-
    bfs([(SX,SY)], [(SX,SY)-none], N, Maze, (GX,GY), Parents),
    build_path((GX,GY), Parents, Rev),
    reverse(Rev, Path).

% Core BFS implementation
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
next_move((X,Y), (NX,NY), N, Maze, Parents) :-
    member((DX,DY), [(1,0),(-1,0),(0,1),(0,-1)]),
    NX is X+DX, NY is Y+DY,
    valid_move(Maze, N, NX, NY),
    \+ memberchk((NX,NY)-_, Parents).

% Build the path from the Parents mapping
build_path((X,Y), Parents, [(X,Y)|Rest]) :-
    memberchk((X,Y)-Parent, Parents),
    ( Parent == none ->
        Rest = []
    ;   build_path(Parent, Parents, Rest)
    ).