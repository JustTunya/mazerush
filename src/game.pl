start :-
    Maze = [
        ['#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
        ['#', '.', '.', '.', '#', '.', '.', '.', '.', '#'],
        ['#', '.', '#', '.', '#', '.', '#', '#', '.', '#'],
        ['#', '.', '#', '.', '.', '.', '.', '#', '.', '#'],
        ['#', '.', '#', '#', '#', '#', '.', '#', '.', '#'],
        ['#', '.', '.', '.', '.', '#', '.', '#', '.', '#'],
        ['#', '.', '#', '#', '.', '#', '.', '#', '.', '#'],
        ['#', '.', '.', '#', '.', '#', '.', '#', '.', '#'],
        ['#', '#', '.', '#', '.', '.', '.', '#', '.', '#'],
        [' ', '#', '#', '#', '#', '#', '#', '#', '#', '#']
    ],
    StartX = 2, StartY = 2,
    GoalX = 9, GoalY = 9,
    game_loop(Maze, 0, StartX, StartY, GoalX, GoalY).

% !-- GAME LOGIC --!

game_loop(Maze, Moves, CurrentX, CurrentY, GoalX, GoalY) :-
    length(Maze, N),
    draw_maze(Maze, N, CurrentX, CurrentY, GoalX, GoalY),
    (goal_reached(CurrentX, CurrentY, GoalX, GoalY) ->
        nl, write('Congratulations! You reached the goal in \033[33m'), write(Moves), write('\033[0m moves!'), nl, nl, !
    ;   draw_ui(Moves),
        get_single_char(Code),
        code_to_char(Code, Direction),
        move(Maze, N, Direction, Moves, NewMoves, CurrentX, CurrentY, NewX, NewY),
        game_loop(Maze, NewMoves, NewX, NewY, GoalX, GoalY)
    ), !.

goal_reached(X, Y, X, Y).

% !-- DRAWING TO COMMAND LINE --!

draw_maze(Maze, N, PlayerX, PlayerY, GoalX, GoalY) :-
    nl, nl,
    forall(between(1, N, I),
        (forall(between(1, N, J),
            (get_cell(Maze, J, I, Cell),
            draw_cell(Cell, PlayerX, PlayerY, GoalX, GoalY, J, I))
        ),
        nl)
    ).

% Usage: draw_cell(Symbol, PlayerX, PlayerY, GoalX, GoalY, CellX, CellY)
draw_cell(_, X, Y, _, _, X, Y) :-
    write('\033[33m'), put(0x25C9), write('\033[0m '), !. % Unicode value of ◉ (PLAYER)
draw_cell(_, _, _, X, Y, X, Y) :-
    write('\033[34m'), put(0x2691), write('\033[0m '), !. % Unicode value of ⚐ (GOAL)
draw_cell('.', _, _, _, _, _, _) :-
    write('\033[30m'), put(0x26F6), write('\033[0m '), !. % Unicode value of ⛶ (PATH)
draw_cell('#', _, _, _, _, _, _) :-
    write('\033[31m'), put(0x25A8), write('\033[0m '), !. % Unicode value of ▨ (WALL)
draw_cell(_, _, _, _, _, _, _) :-
    write('  '). % fallback - empty space

draw_ui(Moves) :-
    nl,
    write('Controls: w \033[30m- up\033[0m, a \033[30m- left\033[0m, s \033[30m- down\033[0m, d \033[30m- right\033[0m'), nl,
    write('Moves made: \033[33m'), write(Moves), write('\033[0m'), 
    nl, nl.

% !-- PLAYERS MOVEMENT --!

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


% ?-- EXAMPLES --?
% game_loop([['#', '#', '#', '#', '#'],['#', '.', '.', '.', '#'],['#', '.', '#', '.', '#'],['#', '.', '.', '.', '#'],['#', '#', '#', '#', '#']], 0, 2, 2, 4, 4).