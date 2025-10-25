menu:-
    print_ascii_art,
    print_instructions,
    choose_difficulty(N),
    write(N),
    generate_maze(N, Maze),
    start_game(N, Maze).

choose_difficulty(N):-
    write('a. Easy'), nl,
    write('b. Medium'), nl,
    write('c. Hard'), nl,
    write('Choose a difficulty!'),
    get_single_char(Code),
    char_code(Difficulty, Code), % converting ascii code to Char
    ( difficulty(Difficulty, N) -> true
    ; write('Invalid choice, try again.'), nl,
      choose_difficulty(N)
    ).

difficulty(a, 5).
difficulty(b, 10).
difficulty(c, 15).

print_instructions:-
    nl.

print_ascii_art:-
    write('                                               __  '), nl,
    write('   ____ ___  ____ _____  ___  _______  _______/ /_ '), nl,
    write('  / __ `__ \\/ __ `/_  / / _ \\/ ___/ / / / ___/ __ \\'), nl,
    write(' / / / / / / /_/ / / /_/  __/ /  / /_/ (__  ) / / /'), nl,
    write('/_/ /_/ /_/\\__,_/ /___/\\___/_/   \\__,_/____/_/ /_/ '), nl,
    write('                                                    '), nl.
