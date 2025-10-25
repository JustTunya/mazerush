% ?- game_loop([['#', '#', '#', '#', '#'],['#', '.', '.', '.', '#'],['#', '.', '#', '.', '#'],['#', '.', '.', '.', '#'],['#', '#', '#', '#', '#']], 2, 2, 4, 4).

start_game(N, Maze):-
    random_valid_cell(N, Maze, StartX, StartY),
    random_valid_cell(N, Maze, GoalX, GoalY),
    game_loop(Maze, StartX, StartY, GoalX, GoalY).

random_valid_cell(N, Maze, RowIndex, ColumnIndex):-
    random_between(1, N, RowIndex_prime), RowTemp is 2 * RowIndex_prime - 1,
    random_between(1, N, ColumnIndex_prime), ColumnTemp is 2 * ColumnIndex_prime - 1,
    wall_cell(WallCell),
    get_char(Maze, RowTemp, ColumnTemp, Cell),
    ( Cell \= WallCell -> RowIndex = RowTemp, ColumnIndex = ColumnTemp
    ; random_valid_cell(N, Maze, RowIndex, ColumnIndex)
    ).

game_loop(Maze, CurrentX, CurrentY, GoalX, GoalY) :-
    length(Maze, N),
    draw_maze(Maze, N, CurrentX, CurrentY, GoalX, GoalY),
    (reachedGoal(CurrentX, CurrentY, GoalX, GoalY) ->
        write('You reached the goal!'), !
    ;
        write('Enter move (w/a/s/d): '),
        get_single_char(Code),
        get_direction(Code, Direction),
        move(Maze, N, Direction, CurrentX, CurrentY, NewX, NewY),
        game_loop(Maze, NewX, NewY, GoalX, GoalY)
    ), !.

get_char(Maze, X, Y, Cell) :-
    nth1(Y, Maze, Row),
    nth1(X, Row, Cell).

inside(N, X, Y) :-
    between(1, N, X),
    between(1, N, Y).

valid_move(Maze, N, X, Y) :-
    inside(N, X, Y),
    get_char(Maze, X, Y, Cell),
    Cell \= '#'.

% draw_cell(Symbol, PlayerX, PlayerY, GoalX, GoalY, CellX, CellY)
draw_cell(_, PlayerX, PlayerY, _, _, PlayerX, PlayerY):-
    write('P'), !.
draw_cell(_, _, _, GoalX, GoalY, GoalX, GoalY):-
    write('G'), !.
draw_cell(Symbol, _, _, _, _, _, _):-
    write(Symbol), !.

draw_maze(Maze, N, PlayerX, PlayerY, GoalX, GoalY):-
    nl,
    forall(between(1, N, I),
        (forall(between(1, N, J),
            (get_char(Maze, I, J, Cell),
            draw_cell(Cell, PlayerX, PlayerY, GoalX, GoalY, I, J))
        ),
        nl)
    ).

step(w, X, Y, X_prime, Y):-
    X_prime is X - 1.
step(a, X, Y, X, Y_prime):-
    Y_prime is Y - 1.
step(s, X, Y, X_prime, Y):-
    X_prime is X + 1.
step(d, X, Y, X, Y_prime):-
    Y_prime is Y + 1.
step(_, X, Y, X, Y). % stays at the same place

move(Maze, N, Direction, CurrentX, CurrentY, NewX, NewY):-
    step(Direction, CurrentX, CurrentY, TempX, TempY),
    (valid_move(Maze, N, TempX, TempY) ->
        (NewX = TempX, NewY = TempY)
    ;   (NewX = CurrentX, NewY = CurrentY)
    ).

%get_direction(Char, Direction):-
%UPPERCASE
get_direction(87, w).
get_direction(65, a).
get_direction(83, s).
get_direction(68, d).
%LOWERCASE
get_direction(119, w).
get_direction(97, a).
get_direction(115, s).
get_direction(100, d).

reachedGoal(X, Y, X, Y).