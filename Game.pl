print_n(S, 1) :- put_char(S).
print_n(S, N) :- N > 0, F is N - 1, print_n(S, F), put_char(S).

print_text+(T, S, P) :- put_char(S), space(P), text(T), space(P), put_char(' '), put_char(S).
print_text(T, S, P) :- put_char(S), space(P), text(T), space(P), put_char(S).
space(1) :- put_char(' ').
space(P) :- P > 1, put_char(' '), F is P - 1, space(F).
text([]).
text([H|T]) :- put_code(H), text(T).

game :- horizontal("1 - START",'X',3), blanck("1 - START",'X',3),
        print_text("1 - START",'X',3), nl, blanck("1 - START",'X',3),
        print_text("2 - RULES",'X',3), nl, blanck("1 - START",'X',3),
        print_text+("0 - EXIT",'X',3), nl, blanck("1 - START",'X',3),
        horizontal("1 - START",'X',3),nl, read(N), choice(N).
horizontal(T, S, P) :- barrier(S,P), words(T,S), barrier(S,P), nl.
barrier(S,0) :- put_char(S).
barrier(S,P) :- P > 0, put_char(S), F is P - 1, barrier(S,F).
words([],_).
words([_|T],S) :- put_char(S), words(T,S).
blanck(T, S, P) :- put_char(S), space(P), words(T,' '), space(P), put_char(S), nl.

choice(1) :- write('game').
choice(2) :- write('MOVE - On each turn, each player must do one of the following actions:'),nl,
	write('Drop a stone into an empty cell.'),nl,
	write('Move a stone into an adjacent (orthogonal or diagonal) empty cell.'),nl,
	write('Jump over an enemy stone landing on the immediate empty cell, and capturing it.'),nl,nl,
	write('Jumps are mandatory and take precedence over drops and moves.'),nl,
	write('Multiple jumps, if possible, are required (no max capture rule)'),nl,nl,
	write('GOAL - Wins the player that makes an orthogonal or diagonal 3 in-a-row, or captures 6 enemy stones.'),nl, read(_), game.
choice(0).
choice(_) :- nl,game.

arena_x([H],_) :- write(H),nl.
arena_x([H|T],3) :- write(H),nl,arena_y,nl,arena_x(T,0).
arena_x([H|T],A) :- B is A+1, write(H), write('--'), arena_x(T,B).

arena_y :- write('|  |  |  |').

arena(A,X,Y) :- remaining('x',X), write('   '), remaining('o',Y), nl, arena_x(A,0), nl.

remaining(_,0).
remaining(X,Y) :- write(X), F is Y-1, remaining(X,F).

piece([_|T],P,0,[P|T]).
piece([H|T],P,X,[H|A]) :- X > 0, Y is X-1, piece(T,P,Y,A).

remove_piece([_|T],0,[' '|T]).
remove_piece([H|T],X,[H|A]) :- X > 0, F is X-1, remove_piece(T,F,A).

move(A,P,X,Y,B) :- remove_piece(A,X,D), piece(D,P,Y,B).


eat(A,P,X,Y,B) :- X > Y, X-Y < 3, R is Y + 1, eat1(A,Y,D,R,E,P,X,B).
eat(A,P,X,Y,B) :- X < Y, Y-X < 3, R is Y - 1, eat1(A,Y,D,R,E,P,X,B).
eat(A,P,X,Y,B) :- X > Y, X-Y > 3, R is Y + 4, eat1(A,Y,D,R,E,P,X,B).
eat(A,P,X,Y,B) :- X < Y, Y-X > 3, R is Y - 4, eat1(A,Y,D,R,E,P,X,B).

eat1(A,Y,D,R,E,P,X,B) :- remove_piece(A,Y,D), remove_piece(D,R,E), piece(E,P,X,B), arena(B,1,1).






