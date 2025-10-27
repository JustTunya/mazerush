start(N, Maze, K) :-
    random_valid_cell(Maze, N, StartX, StartY),
    random_valid_cell(Maze, N, GoalX, GoalY),
    make_enemies(K, Maze, N, Enemies),
    create_interface(2*N+2, 2*N+2),
    game_loop(Maze, 0, StartX, StartY, GoalX, GoalY, Enemies).

% !-- GAME LOGIC --!

game_loop(Maze, Moves, PX, PY, GX, GY, Enemies) :-
    length(Maze, N),
    draw_cmd_maze(Maze, N, PX, PY, GX, GY, Enemies),
    draw_gui_maze(Maze, N, PX, PY, GX, GY, Enemies),
    (   goal_reached(PX, PY, GX, GY) ->
            nl, write('Congratulations! You reached the goal in \033[33m'),
            write(Moves), write('\033[0m moves!'), nl, nl, !
    ;   enemy_collision(PX, PY, Enemies) ->
            nl, write('\033[31mYou were caught by the enemy!\033[0m'), nl, !
    ;   draw_ui(Moves),
        get_single_char(Code),
        code_to_char(Code, Direction),
        move(Maze, N, Direction, Moves, NewMoves, PX, PY, NPX, NPY),
        move_enemies(Maze, N, Enemies, NPX, NPY, NewEnemies),
        game_loop(Maze, NewMoves, NPX, NPY, GX, GY, NewEnemies)
    ), !.

goal_reached(X, Y, X, Y).

% !-- DRAWING TO COMMAND LINE --!

draw_cmd_maze(Maze, N, PlayerX, PlayerY, GoalX, GoalY, Enemies) :-
    nl, nl,
    forall(between(1, N, I),
        (forall(between(1, N, J),
            (get_cell(Maze, J, I, Cell),
             draw_cell(Cell, PlayerX, PlayerY, GoalX, GoalY, Enemies, J, I))
        ),
        nl)
    ).

% Usage: draw_cell(Symbol, PlayerX, PlayerY, GoalX, GoalY, CellX, CellY)
draw_cell(_, X, Y, _, _, _, X, Y) :-
    write('\033[33m'), put(0x25C9), write('\033[0m '), !. % Unicode value of ◉ (PLAYER)
draw_cell(_, _, _, X, Y, _, X, Y) :-
    write('\033[34m'), put(0x2691), write('\033[0m '), !. % Unicode value of ⚐ (GOAL)
draw_cell(_, _, _, _, _, Enemies, X, Y) :-
    member((X,Y), Enemies),
    write('\033[35m'), put(0x2BBF), write('\033[0m '), !. % Unicode value of ⮿ (ENEMY)
draw_cell('.', _, _, _, _, _, _, _) :-
    write('\033[30m'), put(0x26F6), write('\033[0m '), !. % Unicode value of ⛶ (PATH)
draw_cell('#', _, _, _, _, _, _, _) :-
    write('\033[31m'), put(0x25A8), write('\033[0m '), !. % Unicode value of ▨ (WALL)
draw_cell(_, _, _, _, _, _, _, _) :-
    write('  '). % fallback - empty space

draw_ui(Moves) :-
    nl,
    write('Controls: w \033[30m- up\033[0m, a \033[30m- left\033[0m, s \033[30m- down\033[0m, d \033[30m- right\033[0m'), nl,
    write('Moves made: \033[33m'), write(Moves), write('\033[0m'), 
    nl, nl.

% !-- PLAYERS MOVEMENT --!

% Top edge -> wrap to bottom if bottom is path
move(Maze, N, w, Curr, New, X, 1, X, N) :-
    path_cell(Maze, X, N),
    New is Curr + 1.
% Bottom edge -> wrap to top if top is path
move(Maze, N, s, Curr, New, X, N, X, 1) :-
    path_cell(Maze, X, 1),
    New is Curr + 1.
% Left edge -> wrap to right if rightmost is path
move(Maze, N, a, Curr, New, 1, Y, N, Y) :-
    path_cell(Maze, N, Y),
    New is Curr + 1.
% Right edge -> wrap to left if leftmost is path
move(Maze, N, d, Curr, New, N, Y, 1, Y) :-
    path_cell(Maze, 1, Y),
    New is Curr + 1.
% Normal movement
move(Maze, N, Direction, CurrentMoves, NewMoves, CurrentX, CurrentY, NewX, NewY) :-
    step(Direction, CurrentX, CurrentY, TempX, TempY),
    (valid_move(Maze, N, TempX, TempY) ->
        (NewX = TempX, NewY = TempY, NewMoves is CurrentMoves + 1)
    ;   (NewX = CurrentX, NewY = CurrentY, NewMoves is CurrentMoves)
    ).

% Usage: step(Direction, CurrentX, CurrentY, NewX, NewY)
step(w, X, Y, X, Y_prime) :-
    Y_prime is Y - 1.
step(a, X, Y, X_prime, Y) :-
    X_prime is X - 1.
step(s, X, Y, X, Y_prime) :-
    Y_prime is Y + 1.
step(d, X, Y, X_prime, Y) :-
    X_prime is X + 1.
step(_, X, Y, X, Y). % fallback - no movement

valid_move(Maze, N, X, Y) :-
    between(1, N, X),
    between(1, N, Y),
    get_cell(Maze, X, Y, Cell),
    Cell \= '#'.

% !-- UTILITIES --!

path_cell(Maze, X, Y) :- get_cell(Maze, X, Y, '.').

get_cell(Maze, X, Y, Cell) :-
    nth1(Y, Maze, Row),
    nth1(X, Row, Cell).

code_to_char(Code, Char) :-
    char_code(Char, Code),
    ( Code >= 65, Code =< 90 ->
        LowerCode is Code + 32
    ;   LowerCode = Code
    ),
    char_code(Char, LowerCode).

random_valid_cell(Maze, N, RowIndex, ColumnIndex):-
    random_between(1, N, RowIndex_prime), RowTemp is 2 * RowIndex_prime - 1,
    random_between(1, N, ColumnIndex_prime), ColumnTemp is 2 * ColumnIndex_prime - 1,
    wall_cell(WallCell),
    get_cell(Maze, RowTemp, ColumnTemp, Cell),
    ( Cell \= WallCell -> RowIndex = RowTemp, ColumnIndex = ColumnTemp
    ; random_valid_cell(N, Maze, RowIndex, ColumnIndex)
    ).
