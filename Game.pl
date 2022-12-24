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


eat(A,P,X,Y,B) :- X > Y, X-Y < 3, R is Y + 1, eat1(A,Y,R,P,X,B).
eat(A,P,X,Y,B) :- X < Y, Y-X < 3, R is Y - 1, eat1(A,Y,R,P,X,B).
eat(A,P,X,Y,B) :- X > Y, X-Y > 3, R is Y + 4, eat1(A,Y,R,P,X,B).
eat(A,P,X,Y,B) :- X < Y, Y-X > 3, R is Y - 4, eat1(A,Y,R,P,X,B).

eat1(A,Y,R,P,X,B) :- remove_piece(A,Y,D), remove_piece(D,R,E), piece(E,P,X,B), arena(B,1,1).

amount([],0,_).
amount([H|T],D,H) :- amount(T,F,H), D is F+1.
amount([H|T],D,X) :- X\=H, amount(T,D,X).

end_piece(A,X,O) :- amount(A,D,'x'), amount(A,F,'o'), D+X>2, F+O>2, write('continue').
end_piece(A,X,_) :- amount(A,D,'x'), D+X<3, write('o wins').
end_piece(A,_,O) :- amount(A,F,'o'), F+O<3, write('x wins').

end_3row([],_) :- write('continue').
end_3row(['o'|T],X) :- X < 2, what_piece(T,0,'o'), what_piece(T,1,'o'), write('o wins').
end_3row(['o'|T],X) :- X < 2, what_piece(T,3,'o'), what_piece(T,7,'o'), write('o wins').
end_3row(['o'|T],X) :- X < 2, what_piece(T,4,'o'), what_piece(T,9,'o'), write('o wins').
end_3row(['x'|T],X) :- X < 2, what_piece(T,0,'x'), what_piece(T,1,'x'), write('x wins').
end_3row(['x'|T],X) :- X < 2, what_piece(T,3,'x'), what_piece(T,7,'x'), write('x wins').
end_3row(['x'|T],X) :- X < 2, what_piece(T,4,'x'), what_piece(T,9,'x'), write('x wins').
end_3row([_|T],3) :- F is 0, end_3row(T,F).
end_3row([_|T],X) :- X < 3, F is X+1, end_3row(T,F).

what_piece([H|_],0,H).
what_piece([_|T],X,D) :- X > 0, F is X-1, what_piece(T,F,D).

eat_possible_check([],_,_) :- write('nothing to eat').
eat_possible_check([H|T],H,X) :- X < 2, H=='o', what_piece(T,0,'x'), what_piece(T,1,' '), write('o can eat').
eat_possible_check([H|T],H,X) :- X < 2, H=='o', what_piece(T,3,'x'), what_piece(T,7,' '), write('o can eat').
eat_possible_check([H|T],H,X) :- X < 2, H=='o', what_piece(T,4,'x'), what_piece(T,9,' '), write('o can eat').
eat_possible_check([H|T],H,X) :- X < 2, H=='x', what_piece(T,0,'o'), what_piece(T,1,' '), write('x can eat').
eat_possible_check([H|T],H,X) :- X < 2, H=='x', what_piece(T,3,'o'), what_piece(T,7,' '), write('x can eat').
eat_possible_check([H|T],H,X) :- X < 2, H=='x', what_piece(T,4,'o'), what_piece(T,9,' '), write('x can eat').
eat_possible_check([_|T],H,3) :- F is 0, eat_possible_check(T,H,F).
eat_possible_check([_|T],H,X) :- X < 3, F is X+1, eat_possible_check(T,H,F).

reverse(List1,List2):- reverse_aux(List1,[],List2).
reverse_aux([X|XS],R,A):- reverse_aux(XS,[X|R],A).
reverse_aux([],A,A).

eat_possible(A,X) :- eat_possible_check(A,X,0), nl, reverse(A,B), eat_possible_check(B,X,0).






