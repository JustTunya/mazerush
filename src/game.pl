get_char(Maze, X, Y, Cell) :-
    nth1(Y, Maze, Row),
    nth1(X, Row, Cell).

inside(N, X, Y) :-
    X >= 0, X < N,
    Y >= 0, Y < N.

valid_move(Maze, N, X, Y) :-
    inside(N, X, Y),
    get_char(Maze, X, Y, Cell),
    Cell \= '#'.