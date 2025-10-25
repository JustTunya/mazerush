% make_grid(+N, -Grid)
make_grid(N, Grid):- % We are making a grid full of walls first
    make_rows(N, N, Grid). 

% make_rows(+NumberOfRows, +NumberOfColumns, -Grid).
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
