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

% draw_cell(Symbol, PlayerX, PlayerY, CellX, CellY)
draw_cell(_, X, Y, X, Y):-
    write('P'), !.
draw_cell(Symbol, _, _, _, _):-
    write(Symbol), !.

draw_maze(Maze, N, PlayerX, PlayerY):-
    forall(between(1, N, I),
        (forall(between(1, N, J),
            (get_char(Maze, I, J, Cell),
            draw_cell(Cell, PlayerX, PlayerY, I, J))
        ),
        nl)
    ).

step(_, X, Y, X, Y). % stays at the same place
step(w, X, Y, X, Y_prime):-
    Y_prime is Y - 1.
step(a, X, Y, X_prime, Y):-
    X_prime is X - 1.
step(s, X, Y, X, Y_prime):-
    Y_prime is Y + 1.
step(d, X, Y, X_prime, Y):-
    X_prime is X + 1.