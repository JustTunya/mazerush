:- use_module(library(pce)).
:- dynamic canvas/1.
:- dynamic cell_size/1.

cell_size(64).

start :-
    Maze = [
        ['#','#','#','#','#'],
        ['#','.','.','.','#'],
        ['#','.','#','.','#'],
        ['#','.','.','.','#'],
        ['#','#','#','#','#']
    ],
    StartX = 2, StartY = 2,
    GoalX = 4, GoalY = 4,
    cell_size(S),
    create_interface(5, 5, S),
    game_loop(Maze, 0, StartX, StartY, GoalX, GoalY).

% !-- GUI --!

create_interface(Width, Height, Cell) :-
    WinW is Width * Cell,
    WinH is Height * Cell,
    new(Window, picture('MazeRush', size(WinW, WinH))),
    send(Window, open),
    retractall(canvas(_)),
    assertz(canvas(Window)),
    retractall(cell_size(_)),
    assertz(cell_size(Cell)).

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
    ( canvas(Window) -> send(Window, clear) ; true ),
    % 1) PATH and GOAL
    forall(between(1, N, I),
      forall(between(1, N, J),
        ( get_cell(Maze, J, I, C),
          draw_base(C, GoalX, GoalY, J, I)
        ))),
    % 2) PLAYER
    draw_player(PlayerX, PlayerY),
    % 3) WALL
    forall(between(1, N, I),
      forall(between(1, N, J),
        ( get_cell(Maze, J, I, C),
          draw_wall(C, J, I)
        ))).

% Usage: draw_cell(Symbol, PlayerX, PlayerY, GoalX, GoalY, CellX, CellY)
draw_base(_, GX, GY, GX, GY) :- % goal
    canvas(W), cell_size(S),
    X0 is (GX-1)*S, Y0 is (GY-1)*S,
    new(B, bitmap('assets/path.xpm')),
    send(W, display, B, point(X0, Y0)), !.
draw_base('.', _, _, X, Y) :- % path
    canvas(W), cell_size(S),
    X0 is (X-1)*S, Y0 is (Y-1)*S,
    new(B, bitmap('assets/path.xpm')),
    send(W, display, B, point(X0, Y0)), !.
draw_base(_, _, _, _, _) :- true. % fallback

draw_player(X, Y) :- % player
    canvas(W), cell_size(S),
    X0 is (X-1)*S, Y0 is (Y-1)*S,
    new(B, bitmap('assets/player.xpm')),
    send(W, display, B, point(X0, Y0)).

draw_wall('#', X, Y) :-                   % wall
    canvas(W), cell_size(S),
    X0 is (X-1)*S, Y0 is (Y-1)*S,
    new(B, bitmap('assets/wall.xpm')),
    send(W, display, B, point(X0, Y0)), !.
draw_wall(_, _, _) :- true.

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