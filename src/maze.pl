% Defining symbolic constants
empty_cell('.').
wall_cell('#').
player_cell('P').
goal_cell('G').

% First we make a grid full of walls, basically a maze you cannot walk anywhere

% make_grid(+N, -Grid)
make_grid(N, Grid):-
    make_rows(N, N, Grid). 

% make_rows(+NumberOfRows, +NumberOfColumns, -Grid)
make_rows(0, _, []).
make_rows(RowIndex, N, [Row|Rest]):-
    RowIndex > 0,
    make_row(N, Row),
    RowIndex_prime is RowIndex - 1,
    make_rows(RowIndex_prime, N, Rest).

% ?- make_rows(5, 5, Grid). (This call is equivalent to make_grid(5, Grid).)
% Grid = [[#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #|...]]
% false.

% make_row(+NumberOfColumns, -Row)
make_row(0, []).
make_row(N, [WallCell|Rest]):-
    wall_cell(WallCell), % using our defined symbolic constant
    N > 0,
    N_prime is N - 1,
    make_row(N_prime, Rest).

% ?- make_row(5, Sor).
% Sor = [#, #, #, #, #] ;
% false.

% Now we want a function that changes a single cell in the maze given by an index

% set_cell(+Grid, +RowIndex, +ColumnIndex, +NewCell, -NewGrid)
set_cell(Grid, RowIndex, ColumnIndex, NewCell, NewGrid):- % here we actually dont change our grid but recreate it with a one-cell change
    nth1(RowIndex, Grid, Row),
    replace_in_list(Row, ColumnIndex, NewCell, NewRow),
    replace_in_list(Grid, RowIndex, NewRow, NewGrid). % this works because maze is represented as an array of arrays 
    
% ?- make_grid(5, Grid), set_cell(Grid, 1, 1, x, NewGrid).
% Grid = [[#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #|...]],
% NewGrid = [[x, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #|...]] ;
% false.

% replace_in_list(+List, +Index, +NewElement, -NewList)
replace_in_list([_|Rest], 1, NewElement, [NewElement|Rest]). % 1 is just the stopping point of the countdown, because walk the list recursively
replace_in_list([X|Rest], Index, NewElement, [X|NewRest]):-
    Index > 1,
    Index_prime is Index - 1,
    replace_in_list(Rest, Index_prime, NewElement, NewRest).
    
% ?- replace_in_list([a,b,c,d], 3, x, NewList).
% NewList = [a, b, x, d] ;
% false.

% print_grid(+Grid)
print_grid([]).
print_grid([Row|Rest]):-
    print_row(Row),
    nl,
    print_grid(Rest).

% print_row(+Row)
print_row([]).
print_row([Cell|Rest]):-
    write(Cell),
    print_row(Rest).

% ?- make_grid(5, Grid), set_cell(Grid, 1, 1, x, NewGrid), print_grid(NewGrid).
% x####
% #####
% #####
% #####
% #####
% Grid = [[#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #|...]],
% NewGrid = [[x, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #|...]] ;
% false.

% random_cell(+N, -RowIndex, -ColumnIndex)
random_cell(N, RowIndex, ColumnIndex):-
    random_between(1, N, RowIndex), % Generating random number for rows
    random_between(1, N, ColumnIndex). % Generating random number for columns

% ?- make_grid(5, Grid), random_cell(5, RowIndex, ColumnIndex), set_cell(Grid, RowIndex, ColumnIndex, x, NewGrid), print_grid(NewGrid).
% #####
% #####
% #####
% #####
% ##x##
% Grid = [[#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #|...]],
% RowIndex = 5,
% ColumnIndex = 3,
% NewGrid = [[#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, x, #|...]] ;
% false.

% We want to get all valid (not out of bounds) neighbours of a cell

% neighbours(+RowIndex, +ColumnIndex, +N, -Neighbours):-
neighbours(RowIndex, ColumnIndex, N, Neighbours):- % Neighbours will be a list in the form of [(Row1, Column1), [(Row2, Column2)], ...], or [] if there is no neighbour, the order is up, right, down, left (clockwise)
    neighbours_up(RowIndex, ColumnIndex, N, UpNeighbours),
    neighbours_down(RowIndex, ColumnIndex, N, DownNeighbours),
    neighbours_right(RowIndex, ColumnIndex, N, RightNeighbours),
    neighbours_left(RowIndex, ColumnIndex, N, LeftNeighbours),
    append([UpNeighbours, RightNeighbours, DownNeighbours, LeftNeighbours], Neighbours).

% ?- neighbours(3, 3, 5, L).
% L = [(2, 3), (3, 4), (4, 3), (3, 2)].

% neighbours_up(+RowIndex, +ColumnIndex, +N, -NeighbourList)
neighbours_up(RowIndex, ColumnIndex, _, [(RowIndex_prime, ColumnIndex)]):-
    RowIndex_prime is RowIndex - 2,
    RowIndex_prime >= 1, !.
neighbours_up(_, _, _, []). % Out of bounds, no neighbour

% neighbours_right(+RowIndex, +ColumnIndex, +N, -NeighbourList)
neighbours_right(RowIndex, ColumnIndex, N, [(RowIndex, ColumnIndex_prime)]):-
    ColumnIndex_prime is ColumnIndex + 2,
    ColumnIndex_prime =< N, !.
neighbours_right(_, _, _, []). % Out of bounds, no neighbour

% neighbours_down(+RowIndex, +ColumnIndex, +N, -NeighbourList)
neighbours_down(RowIndex, ColumnIndex, N, [(RowIndex_prime, ColumnIndex)]):-
    RowIndex_prime is RowIndex + 2,
    RowIndex_prime =< N, !.
neighbours_down(_, _, _, []). % Out of bounds, no neighbour

% neighbours_left(+RowIndex, +ColumnIndex, +N, -NeighbourList)
neighbours_left(RowIndex, ColumnIndex, _, [(RowIndex, ColumnIndex_prime)]):-
    ColumnIndex_prime is ColumnIndex - 2,
    ColumnIndex_prime >= 1, !.
neighbours_left(_, _, _, []). % Out of bounds, no neighbour

% carve_path(+Grid, +RowIndex, +ColumnIndex, -NewGrid):-
carve_path(Grid, RowIndex, ColumnIndex, NewGrid):-
    empty_cell(EmptyCell), % using our defined symbolic constant
    set_cell(Grid, RowIndex, ColumnIndex, EmptyCell, NewGrid).

% ?- make_grid(5, Grid), random_cell(5, RowIndex, ColumnIndex), carve_path(Grid, RowIndex, ColumnIndex, NewGrid), print_grid(NewGrid).
% #####
% ##.##
% #####
% #####
% #####
% Grid = [[#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #|...]],
% RowIndex = 2,
% ColumnIndex = 3,
% NewGrid = [[#, #, #, #, #], [#, #, '.', #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #|...]] ;
% false.

% when moving from one cell to the other we need to carve the path between them, not just the target cell
% mid_cell((+CurrentRow, +CurrentColumn), (+TargetRow, +TargetColumn), (-MiddleRow, -MiddleColumn))
mid_cell((CurrentRow, CurrentColumn), (TargetRow, TargetColumn), (MiddleRow, MiddleColumn)):-
    MiddleRow is (CurrentRow + TargetRow) // 2, % we calc the average of them
    MiddleColumn is (CurrentColumn + TargetColumn) // 2.

% ?- mid_cell((3,4), (3,6), (MiddleRow, MiddleColumn)).
% MiddleRow = 3,
% MiddleColumn = 5.

% dfs_carve(+Grid, +Stack, +N, -NewGrid)
dfs_carve(Grid, [], _, Grid). % we are done when the stack gets empty

dfs_carve(Grid, [(RowIndex, ColumnIndex)|RestStack], N, NewGrid):-
    neighbours(RowIndex, ColumnIndex, N, Neighbours),
    filter_unvisited(Neighbours, Grid, Unvisited), % we need the unvisited neighbours
    ( Unvisited = [] -> dfs_carve(Grid, RestStack, N, NewGrid) % this means that we reached a dead-end in carving and need to backtrack
    ;
      random_member((RandomRow, RandomColumn), Unvisited),
      mid_cell((RowIndex, ColumnIndex), (RandomRow, RandomColumn), (MiddleRow, MiddleColumn)),
      carve_path(Grid, MiddleRow, MiddleColumn, Grid_prime),
      carve_path(Grid_prime, RandomRow, RandomColumn, GridNext), % carve target cell
      dfs_carve(GridNext, [(RandomRow, RandomColumn), (RowIndex, ColumnIndex)|RestStack], N, NewGrid) % pushing the new cell on top instead of replacing!!!
    ).

%filter unvisited neighbours
% filter_unvisited(+NeighboursList, +Grid, -FilteredNeighboursList)
filter_unvisited([], _, []).
filter_unvisited([(RowIndex, ColumnIndex)|Rest], Grid, [(RowIndex, ColumnIndex)|FilteredRest]):-
    unvisited(Grid, (RowIndex, ColumnIndex)), % we check if the cell at RowIndex, ColumnIndex is visited or not
    filter_unvisited(Rest, Grid, FilteredRest).

filter_unvisited([(RowIndex, ColumnIndex)|Rest], Grid, FilteredNeighboursList):- % we need this case, because we match the string in the parameterlist in the first above
    \+ unvisited(Grid, (RowIndex, ColumnIndex)),
    filter_unvisited(Rest, Grid, FilteredNeighboursList).

% ?- make_grid(5, G), carve_path(G, 2, 3, G2), neighbours(3, 3, 5, N), filter_unvisited(N, G2, U).
% G = [[#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #|...]],
% G2 = [[#, #, #, #, #], [#, #, '.', #, #], [#, #, #, #, #], [#, #, #, #, #], [#, #, #, #|...]],
% N = [(2, 3), (3, 4), (4, 3), (3, 2)],
% U = [(3, 4), (4, 3), (3, 2)] ;
% false.

% unvisited check
unvisited(Grid, (RowIndex, ColumnIndex)):-
    nth1(RowIndex, Grid, Row),
    nth1(ColumnIndex, Row, Cell),
    wall_cell(Cell).

% generate_maze(+N, -Maze)
generate_maze(N, Maze):-
    GridSize is 2*N + 1,
    make_grid(GridSize, Grid),
    % choose random starting cell in odd indices
    random_between(1, N, RandomRowIndex), StartRowIndex is 2 * RandomRowIndex - 1,
    random_between(1, N, RandomColumnIndex), StartColumnIndex is 2 * RandomColumnIndex - 1,
    carve_path(Grid, StartRowIndex, StartColumnIndex, Grid_prime), % carve starting cell
    dfs_carve(Grid_prime, [(StartRowIndex, StartColumnIndex)], GridSize, Maze).
    % print_grid(Maze).
