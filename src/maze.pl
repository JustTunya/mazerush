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
make_row(N, ['#'|Rest]):-
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