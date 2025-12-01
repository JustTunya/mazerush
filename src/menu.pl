% menu
menu:-
    print_ascii_art,
    choose_difficulty(N),
    generate_maze(N, Maze),
    start(N, Maze), !.

% choose_difficulty(-N)
choose_difficulty(N) :-
    write('a. Easy'), nl,
    write('b. Medium'), nl,
    write('c. Hard'), nl,
    write('Choose a difficulty!'), nl,
    get_single_char(Code),
    char_code(Difficulty, Code),
    difficulty_picker(Difficulty, N).

% difficulty_picker(+Difficulty, -N)
difficulty_picker(Difficulty, N) :-
    difficulty(Difficulty, N), !.
difficulty_picker(_, N) :-
    write('Invalid choice, try again.'), nl,
    choose_difficulty(N).

% difficulty(+Difficulty, -N)
difficulty(a, 5).
difficulty(b, 10).
difficulty(c, 15).

% print_ascii_art
print_ascii_art:-
    write('                                               __  '), nl,
    write('   ____ ___  ____ _____  ___  _______  _______/ /_ '), nl,
    write('  / __ `__ \\/ __ `/_  / / _ \\/ ___/ / / / ___/ __ \\'), nl,
    write(' / / / / / / /_/ / / /_/  __/ /  / /_/ (__  ) / / /'), nl,
    write('/_/ /_/ /_/\\__,_/ /___/\\___/_/   \\__,_/____/_/ /_/ '), nl,
    write('                                                    '), nl.
