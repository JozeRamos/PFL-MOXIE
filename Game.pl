?- use_module(library(lists)).














% ----------------------- Menu ---------------------------
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
% ----------------------------------------------------------

% ------------------- Arena building -----------------------

arena_x([H],_) :- write(H),nl.
arena_x([H|T],3) :- write(H),nl,arena_y,nl,arena_x(T,0).
arena_x([H|T],A) :- B is A+1, write(H), write('--'), arena_x(T,B).

arena_y :- write('|  |  |  |').

% controi uma arena usando arena_x e arena_y (ex: arena(['x',' ','o', ' ', 'x','o',' ', 'o','o',' ',' ', ' ', 'x', ' ', ' ','o'], 1, 1) )
arena(A,X,Y) :- remaining('x',X), write('   '), remaining('o',Y), nl, arena_x(A,0), nl.

remaining(_,0).
remaining(X,Y) :- write(X), F is Y-1, remaining(X,F).

% ----------------------------------------------------------

% -------------------- Moves ----------------------

% place piece (dada um lista de chars que representa a arena)
piece([_|T],P,0,[P|T]).
piece([H|T],P,X,[H|A]) :- X > 0, Y is X-1, piece(T,P,Y,A).

% removes piece (dada um lista de chars que representa a arena)  remove_piece(-Map, -Index_of_piece, +New_map)
remove_piece([_|T],0,[' '|T]).
remove_piece([H|T],X,[H|A]) :- X > 0, F is X-1, remove_piece(T,F,A).

% moves piece (dada um lista de chars que representa a arena)
move(A,P,X,Y,B) :- remove_piece(A,X,D), piece(D,P,Y,B).




% eat(Map, Index_eater, Index_food, New_map) (ex: eat(['x', 'o', ' ', ' ','o', 'o',' ', ' ',' ', 'o',' ', ' ',' ', ' ',' ', ' '],0,5,X), eat(X, 10, 9, X1), eat(X1,8,4,X2), eat(X2,0,1,X3). )
eat(A,X,Y,B) :- can_eat(A, X, Y, Z), eat_aux(A, X, Y, Z, B).

% eat_aux(Arena , index peça que vai comer, index peça a ser comida, Peça a comer outra, nova arena)
eat_aux(A,X,Y,Z,B) :- what_piece(A, X, P), remove_piece(A, X, D), remove_piece(D, Y, E), piece(E, P, Z, B), arena(B, 1, 1).

% 
can_eat(A,X,Y,Z) :- eater_move(X, Y, Z), 
						what_piece(A, X, E),
						what_piece(A, Y, C),
						what_piece(A, Z, F),
						F == ' ',
						((E == 'x', C == 'o'); (E == 'o', C == 'x')).

% eater_move(index inicial eater, posiçao comida, index final eater)
eater_move(X, Y, Z) :- between(0, 15, X), between(0, 15, Y),
				((Y-X =:= 1, Z is Y + 1);
				(Y-X =:= 3, Z is Y + 3);
				(Y-X =:= 4, Z is Y + 4);
				(Y-X =:= 5, Z is Y + 5);
				(X-Y =:= 1, Z is Y - 1);
				(X-Y =:= 3, Z is Y - 3);
				(X-Y =:= 4, Z is Y - 4);
				(X-Y =:= 5, Z is Y - 5)),
				between(0,15,Z).

% -------------------------------------------------

% -------------------- Check End ----------------------

end_piece(A,X,O) :- amount(A,D,'x'), amount(A,F,'o'), D+X>2, F+O>2, write('continue').
end_piece(A,X,_) :- amount(A,D,'x'), D+X<3, write('o wins').
end_piece(A,_,O) :- amount(A,F,'o'), F+O<3, write('x wins').

amount([],0,_).
amount([H|T],D,H) :- amount(T,F,H), D is F+1.
amount([H|T],D,X) :- X\=H, amount(T,D,X).


end_3row([],_) :- write('continue').
end_3row(['o'|T],X) :- X < 2, what_piece(T,0,'o'), what_piece(T,1,'o'), write('o wins').
end_3row(['o'|T],X) :- X < 2, what_piece(T,3,'o'), what_piece(T,7,'o'), write('o wins').
end_3row(['o'|T],X) :- X < 2, what_piece(T,4,'o'), what_piece(T,9,'o'), write('o wins').
end_3row(['x'|T],X) :- X < 2, what_piece(T,0,'x'), what_piece(T,1,'x'), write('x wins').
end_3row(['x'|T],X) :- X < 2, what_piece(T,3,'x'), what_piece(T,7,'x'), write('x wins').
end_3row(['x'|T],X) :- X < 2, what_piece(T,4,'x'), what_piece(T,9,'x'), write('x wins').
end_3row([_|T],3) :- F is 0, end_3row(T,F).
end_3row([_|T],X) :- X < 3, F is X+1, end_3row(T,F).

% -----------------------------------------------------

what_piece([H|_],0,H).
what_piece([_|T], X, D) :- X > 0, F is X-1, what_piece(T,F,D).

between(X,Y,V) :- V >= X, V =< Y.

eat_possible(A,X) :- eat_possible_check(A,X,0), nl, reverse(A,B), eat_possible_check(B,X,0).

eat_possible_check([],_,_) :- write('nothing to eat').
eat_possible_check([H|T],H,X) :- X < 2, H=='o', what_piece(T,0,'x'), what_piece(T,1,' '), write('o can eat').
eat_possible_check([H|T],H,X) :- X < 2, H=='o', what_piece(T,3,'x'), what_piece(T,7,' '), write('o can eat').
eat_possible_check([H|T],H,X) :- X < 2, H=='o', what_piece(T,4,'x'), what_piece(T,9,' '), write('o can eat').
eat_possible_check([H|T],H,X) :- X < 2, H=='x', what_piece(T,0,'o'), what_piece(T,1,' '), write('x can eat').
eat_possible_check([H|T],H,X) :- X < 2, H=='x', what_piece(T,3,'o'), what_piece(T,7,' '), write('x can eat').
eat_possible_check([H|T],H,X) :- X < 2, H=='x', what_piece(T,4,'o'), what_piece(T,9,' '), write('x can eat').
eat_possible_check([_|T],H,3) :- F is 0, eat_possible_check(T,H,F).
eat_possible_check([_|T],H,X) :- X < 3, F is X+1, eat_possible_check(T,H,F).




% reverse(List1, List2):- reverse_aux(List1,[],List2). % trocada pelo reverse do pacote lists
% reverse_aux([X|XS], R, A):- reverse_aux(XS,[X|R],A).
% reverse_aux([], A, A).


% ------------------- Valid Moves ------------------------------

% piece_option([Map], RES) jogadas de place pieces possiveis (resultado é uma lista de ints que representam posições na board)

piece_option(Map, Moves) :- findall(I, indexOf(Map,' ', I), Moves).

indexOf([Element|_], Element, 0).  % pode-se se trocar por nth0
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  Index is Index1+1.


% obter todas as jogadas possiveis ([init-end]) de move tendo em conta o player board(4x4)

move_option(Map, Piece, Moves) :- findall(Init-End, valid_move(Map, Piece, Init, End), Moves).

valid_move(Map, Piece, I, E) :- indexOf(Map, ' ', E), 
								indexOf(Map, Piece, I),
								((E =:= I-1, I =\= 4, I=\=8, I=\=12);
								(E =:= I+1, I=\=3, I=\=7, I=\=11);
								(E =:= I+4);
								(E =:= I-4);
								(E =:= I-3, I=\=3, I=\=7, I=\=11, I=\=15); % /^
								(E =:= I-5, I=\=4, I=\=8, I=\=12); % \^
								(E =:= I+5, I=\=3, I=\=7, I=\=11); % \v 
								(E =:= I+3, I=\=12, I=\=8, I=\=4, I=\=0)), % /v
								E >= 0, E =< 16.


% obter todas as jogadas possiveis ([init-end]) de eat tendo em conta o player board(4x4)

% Moves sao os varios moves que todas as Pieces to tipo Piece podem realizar. Assume-se tamanho da board (4x4)
eat_option(Map, Piece, Moves) :- findall(Move, valid_eats(Map,Piece,Move, 15), [Moves|_]).


valid_eat(A,X,Piece,Z) :- what_piece(A, X, Piece),
					(Y is X+1; Y is X-1; Y is X+3; Y is X-3; Y is X+4; Y is X-4; Y is X+5; Y is X-5),
					can_eat(A,X,Y,Z).

valid_eats(_, _, _, (-1)).
valid_eats(A, Piece, Moves, X) :- X >= 0,
								findall(X-Z, valid_eat(A,X,Piece,Z), Move),
								add_lists(Move, Movs, Moves),
								Xs is X-1,
								valid_eats_aux(A, Piece, Movs, Xs).

add_lists([],List2,List2).
add_lists(List1,[],List1).
add_lists([H|T], List2 ,[H|T2]) :- add_lists(T,List2 ,T2).


%valid_eats(A, Piece, H) :- between(0,15,X),
%				what_piece(A, X, Piece),
%				(Y is X+1; Y is X-1; Y is X+3; Y is X-3; Y is X+4; Y is X-4; Y is X+5; Y is X-5),
%				findall(X-Z, can_eat(A,X,Y,Z), H).


% --------------------------------------------------------------