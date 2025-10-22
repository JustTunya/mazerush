game_loop(Maze, CurrentX, CurrentY, GoalX, GoalY) :-
    length(Maze, N),
    draw_maze(Maze, N, CurrentX, CurrentY, GoalX, GoalY),
    (goal_reached(CurrentX, CurrentY, GoalX, GoalY) ->
        write('Congratulations! You reached the goal!'), nl, !
    ;   write('Enter move (w/a/s/d): '),
        get_single_char(Code),
        code_to_char(Code, Direction),
        move(Maze, N, Direction, CurrentX, CurrentY, NewX, NewY),
        game_loop(Maze, NewX, NewY, GoalX, GoalY)
    ), !.

% !-- GAME LOGIC --!

goal_reached(X, Y, X, Y).

% !-- DRAWING THE MAZE --!

draw_maze(Maze, N, PlayerX, PlayerY, GoalX, GoalY) :-
    nl,
    forall(between(1, N, I),
        (forall(between(1, N, J),
            (get_cell(Maze, I, J, Cell),
            draw_cell(Cell, PlayerX, PlayerY, GoalX, GoalY, I, J))
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

% !-- PLAYERS MOVEMENT --!

move(Maze, N, Direction, CurrentX, CurrentY, NewX, NewY) :-
    step(Direction, CurrentX, CurrentY, TempX, TempY),
    (valid_move(Maze, N, TempX, TempY) ->
        (NewX = TempX, NewY = TempY)
    ;   (NewX = CurrentX, NewY = CurrentY)
    ).

% Usage: step(Direction, CurrentX, CurrentY, NewX, NewY)
step(w, X, Y, X_prime, Y) :-
    X_prime is X - 1.
step(a, X, Y, X, Y_prime) :-
    Y_prime is Y - 1.
step(s, X, Y, X_prime, Y) :-
    X_prime is X + 1.
step(d, X, Y, X, Y_prime) :-
    Y_prime is Y + 1.
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

% game_loop([['#', '#', '#', '#', '#'],['#', '.', '.', '.', '#'],['#', '.', '#', '.', '#'],['#', '.', '.', '.', '#'],['#', '#', '#', '#', '#']], 2, 2, 4, 4).