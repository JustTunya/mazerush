:- use_module(library(pce)).

graphic_start_game(N, Maze):-
    random_valid_cell(N, Maze, StartX, StartY),
    random_valid_cell(N, Maze, GoalX, GoalY),
    % Create window once at start
    length(Maze, Rows),
    nth1(1, Maze, FirstRow),
    length(FirstRow, Cols),
    CellSize = 32,
    Width is Cols * CellSize,
    Height is Rows * CellSize,
    new(Window, picture('Maze')),
    send(Window, size, size(Width, Height)),
    send(Window, open),
    graphic_game_loop(Window, Maze, CellSize, StartX, StartY, GoalX, GoalY).

graphic_game_loop(Window, Maze, CellSize, CurrentX, CurrentY, GoalX, GoalY) :-
    format('~n=== GAME LOOP: Player at (~w,~w), Goal at (~w,~w) ===~n', [CurrentX, CurrentY, GoalX, GoalY]),
    length(Maze, N),
    send(Window, clear),
    draw_cells_colors(Window, Maze, 0, CellSize, CurrentX, CurrentY, GoalX, GoalY),
    draw_maze(Maze, N, CurrentX, CurrentY, GoalX, GoalY),
    send(Window, flush),
    (reachedGoal(CurrentX, CurrentY, GoalX, GoalY) ->
        write('You reached the goal!'), nl,
        send(@display, inform, 'Congratulations!\n\nYou reached the goal!'),
        send(Window, destroy), !
    ;
        write('Enter move (w/a/s/d): '),
        get_single_char(Code),
        get_direction(Code, Direction),
        move(Maze, N, Direction, CurrentX, CurrentY, NewX, NewY),
        graphic_game_loop(Window, Maze, CellSize, NewX, NewY, GoalX, GoalY)
    ), !.

% In draw_cells_colors, pass N
draw_cells_colors(Window, Maze, RowIndex, CellSize, PlayerX, PlayerY, GoalX, GoalY) :-
    length(Maze, N),  % Get total rows
    draw_cells_colors_helper(Window, Maze, RowIndex, CellSize, PlayerX, PlayerY, GoalX, GoalY, N).

draw_cells_colors_helper(_, [], _, _, _, _, _, _, _).
draw_cells_colors_helper(Window, [Row|RestRows], RowIndex, CellSize, PlayerX, PlayerY, GoalX, GoalY, N) :-
    length(Row, _),
    RowIndex1 is RowIndex + 1,
    draw_row_colors(Window, Row, RowIndex, 0, CellSize, PlayerX, PlayerY, GoalX, GoalY, RowIndex1, N),
    RowIndex2 is RowIndex + 1,
    draw_cells_colors_helper(Window, RestRows, RowIndex2, CellSize, PlayerX, PlayerY, GoalX, GoalY, N).

draw_row_colors(_, [], _, _, _, _, _, _, _, _, _).
draw_row_colors(Window, [Cell|RestCols], RowIndex, ColIndex, CellSize, PlayerX, PlayerY, GoalX, GoalY, CurrentRowNum, N) :-
    Y is ColIndex * CellSize,      % Column determines Y (vertical)
    X is RowIndex * CellSize,      % Row determines X (horizontal)
    ColIndex1 is ColIndex + 1,
    
    % SWAPPED: PlayerX is COLUMN, PlayerY is ROW
    (CurrentRowNum = PlayerY, ColIndex1 = PlayerX ->
        format('PLAYER MATCH: Row=~w, Col=~w~n', [CurrentRowNum, ColIndex1]),
        Color = colour(red)
    ; CurrentRowNum = GoalY, ColIndex1 = GoalX ->
        format('GOAL MATCH: Row=~w, Col=~w~n', [CurrentRowNum, ColIndex1]),
        Color = colour(green)
    ;
        cell_color(Cell, Color)
    ),
    
    new(Box, box(CellSize, CellSize)),
    send(Box, fill_pattern, Color),
    send(Window, display, Box, point(X,Y)),
    ColIndex2 is ColIndex + 1,
    draw_row_colors(Window, RestCols, RowIndex, ColIndex2, CellSize, PlayerX, PlayerY, GoalX, GoalY, CurrentRowNum, N).

% Map maze characters to XPCE colors
cell_color('#', colour(black)).
cell_color('.', colour(white)).
cell_color('P', colour(red)).
cell_color('G', colour(green)).