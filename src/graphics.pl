:- use_module(library(pce)).
:- dynamic canvas/1.
:- dynamic cell_size/1.

cell_size(32).

create_interface(Width, Height) :-
    cell_size(Cell),
    WinW is Width * Cell,
    WinH is Height * Cell,
    new(Window, picture('MazeRush', size(WinW, WinH))),
    send(Window, open),
    retractall(canvas(_)),
    assertz(canvas(Window)),
    retractall(cell_size(_)),
    assertz(cell_size(Cell)).

draw_gui_maze(Maze, N, PlayerX, PlayerY, GoalX, GoalY, Enemies) :-
    ( canvas(Window) -> send(Window, clear) ; true ),
    % 1) PATH
    forall(between(1, N, I),
      forall(between(1, N, J),
        ( get_cell(Maze, J, I, C),
          draw_path(C, J, I)
        ))),
    % 2) GOAL
    draw_entity(GoalX, GoalY, 'assets/goal.xpm'),
    % 3) PLAYER
    draw_entity(PlayerX, PlayerY, 'assets/player.xpm'),
    % 4) ENEMIES (list)
    draw_enemies(Enemies, 1),
    % 5) WALL
    forall(between(1, N, I),
      forall(between(1, N, J),
        ( get_cell(Maze, J, I, C),
          draw_wall(C, J, I, Maze, N)
        ))).

draw_enemies([], _).
draw_enemies([(X,Y)|T], I) :-
    format(atom(Sprite), 'assets/enemy_~d.xpm', [I]),
    draw_entity(X, Y, Sprite),
    NewI is I + 1,
    draw_enemies(T, NewI).

draw_path('.', X, Y) :-
    canvas(W), cell_size(S),
    X0 is (X-1)*S, Y0 is (Y-1)*S,
    new(B, bitmap('assets/path.xpm')),
    send(W, display, B, point(X0, Y0)), !.
draw_path(_, _, _) :- true.

draw_entity(X, Y, Sprite) :-
    canvas(W), cell_size(S),
    X0 is (X-1)*S, Y0 is (Y-1)*S,
    new(B, bitmap(Sprite)),
    send(W, display, B, point(X0, Y0)).

draw_wall('#', X, Y, Maze, N) :-
    canvas(W), cell_size(S),
    X0 is (X-1)*S, Y0 is (Y-1)*S,
    wall_sprite(Maze, N, X, Y, Sprite),
    new(B, bitmap(Sprite)),
    send(W, display, B, point(X0, Y0)), !.
draw_wall(_, _, _, _, _) :- true.

wall_sprite(Maze, N, X, Y, 'assets/sidewall.xpm') :-
    ( Y =:= N
    ; 
      Y0 is Y + 1,
      Y < N, get_cell(Maze, X, Y0, '.')
    ), !.
wall_sprite(_, _, _, _, 'assets/wall.xpm').