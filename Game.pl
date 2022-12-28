?- use_module(library(lists)).

play :- menu.

% display_game(+GameState)
display_game(GameState) :- arena(GameState, 8, 8).

game :- initial_state(16, GameState),
		game_cycle(GameState).


% game_cycle(GameState-Player):-	game_over(GameState, Winner), !,
%									congratulate(Winner).

game_cycle(GameState):- game_turn(GameState, 'x', NewGameState),
						game_turn(NewGameState, 'o', NewNewGameState),
						game_cycle(NewNewGameState).
						
game_turn(GameState, Player, NewGameState) :- display_game(GameState),
						choose_move(GameState, Player, human, X_move),
						make_move(GameState, X_move, Player, NewGameState).

% initial_state(+Size, -GameState) Size is the number of places on the board. Cria uma board vazia com o size dado.
initial_state(0, []).
initial_state(Size, [' '|T]) :- Size > 0,
								S is Size-1,
								initial_state(S, T).


% move(+GameState, +Move, -NewGameState)
make_move(GameState, Move, Player, NewGameState) :- integer(Move), piece(GameState, Player, Move, NewGameState).
make_move(GameState, X-Y, Player, NewGameState) :- valid_move(GameState,Player,X,Y), move(GameState, X, Y, NewGameState).
make_move(GameState, X-Z, _, NewGameState) :- eat(GameState, X, Z, NewGameState).

% valid_moves(+GameState, +Player, -ListOfMoves)
valid_moves(GameState, Player, ListOfMoves) :- piece_option(GameState, Piece_moves),
												move_option(GameState, Player, Move_moves),
												eat_option(GameState, Player, Eat_moves),
												add_lists(Piece_moves, Move_moves, L),
												add_lists(L, Eat_moves, ListOfMoves).
											   

% game_over(+GameState, -Winner)
% game_over(GameState, Winner).

% value(+GameState, +Player, -Value)



% !!!!!!!!!!!!!!!!!!!!!!! Adicionar informação (tipo "Eat is required" e "Invalid Move") !!!!!!!!!!!!
% choose_move(+GameState, +Player, +Level, -Move)
choose_move(GameState, Player, human, Move) :- repeat, write(Player), write(' turn: '), read(Move),
												valid_moves(GameState, Player, Valid_moves),
												memberchk(Move, Valid_moves),
												eat_option(GameState, Player, Eats),
												(eat_option(GameState, Player, []); memberchk(Move, Eats)).

% choose_move(GameState, Player, 1, Move) :-
% choose_move(GameState, Player, 2, Move) :-

% ----------------------- Menu e I/O ---------------------------

len([],0).
len([_ | T], N) :- len(T, S),
				N is S+1.

repeat_char(_, 0).
repeat_char(C, 1) :- put_char(C).
repeat_char(C, N) :- N > 0, put_char(C), F is N - 1, repeat_char(C, F).

text([]).
text([H|T]) :- put_code(H), text(T).

print_text(T, S, P) :- put_char(S), repeat_char(' ', P), text(T), repeat_char(' ', P), put_char(S).

print_menu_option(T, S, P) :- len(T, N),
						B is N+2*P,
						put_char(S), repeat_char(' ',B), put_char(S), nl,
						print_text(T, S, P), nl,
						put_char(S), repeat_char(' ',B), put_char(S), nl.

menu :- nl, print_text(" MOXIE!",' ', 4), nl,
		repeat_char('X',17), nl,
		print_menu_option("1 - START", 'X', 3),
		print_menu_option("2 - RULES", 'X', 3),
		print_menu_option("0 - EXIT ", 'X', 3),
		repeat_char('X',17), nl, nl,
		text("Your choice: "), read(N), menu_choice(N).



menu_choice(1) :- game, nl. 
menu_choice(2) :- nl, print_text("Rules", '|', 5), nl,
	write('MOVE - On each turn, each player must do one of the following actions:'),nl,
	write('Drop a stone into an empty cell.'), nl,
	write('Move a stone into an adjacent (orthogonal or diagonal) empty cell.'),nl,
	write('Jump over an enemy stone landing on the immediate empty cell, and capturing it.'),nl,nl,
	write('Jumps are mandatory and take precedence over drops and moves.'),nl,
	write('Multiple jumps, if possible, are required (no max capture rule)'),nl,nl,
	write('GOAL - Wins the player that makes an orthogonal or diagonal 3 in-a-row, or captures 6 enemy stones.'), nl, read(_), menu.
menu_choice(0).
menu_choice(_) :- nl, menu.
								


% ----------------------------------------------------------

% ------------------- Arena building -----------------------

arena_x([H],_) :- write(H),nl.
arena_x([H|T],3) :- write(H),nl,arena_y,nl,arena_x(T,0).
arena_x([H|T],A) :- B is A+1, write(H), write('--'), arena_x(T,B).

arena_y :- write('|  |  |  |').

% controi uma arena usando arena_x e arena_y (ex: arena(['x',' ','o', ' ', 'x','o',' ', 'o','o',' ',' ', ' ', 'x', ' ', ' ','o'], 1, 1) )
arena(A,X,O) :- nl, remaining('x',X), write('   '), remaining('o',O), nl, arena_x(A,0), nl.

remaining(_,0).
remaining(X,O) :- write(X), F is O-1, remaining(X,F).

% ----------------------------------------------------------

% -------------------- Moves ----------------------

% place piece (dada um lista de chars que representa a arena)  piece(-Map, -Piece, -Index_of_piece, ?New_map)
piece([_|T], P, 0, [P|T]).
piece([H|T], P, X, [H|A]) :- X > 0, Y is X-1, piece(T,P,Y,A).

% removes piece (dada um lista de chars que representa a arena)  remove_piece(-Map, -Index_of_piece, ?New_map)
remove_piece([_|T],0,[' '|T]).
remove_piece([H|T],X,[H|A]) :- X > 0, F is X-1, remove_piece(T,F,A).

% moves piece (dada um lista de chars que representa a arena)
move(A,X,Y,B) :- what_piece(A, X, P), 
				remove_piece(A,X,D), piece(D,P,Y,B).

% piece eats another piece -> eat(-Map, -Posiçao inicial, -Posiçao final, ?New_map)
% (ex: eat(['x', 'o', ' ', ' ','o', 'o',' ', ' ',' ', 'o',' ', ' ',' ', ' ',' ', ' '],0,10,X), eat(X, 10, 8, X1), eat(X1,8,0,X2), eat(X2,0,2,X3).)
eat(A, X, Z, B) :- calc_eat_move(X, Y, Z), % calculate food position
					move(A, X, Z, D), remove_piece(D, Y, B).


% calc_eat_move(X, Y, Z) (-index inicial eater, -index comida, ?index final eater)
% apenas para boards (4 * Y)
calc_eat_move(X, Y, Z) :- (var(Z) ->
						((Y-X =:= 1, Z is Y + 1, ((Y-3) mod 4) =\= 0);  % calcular Z se estiver em falta
						(Y-X =:= 3, Z is Y + 3, (Y mod 4) =\= 0);
						(Y-X =:= 4, Z is Y + 4);
						(Y-X =:= 5, Z is Y + 5, ((Y-3) mod 4) =\= 0);
						(X-Y =:= 1, Z is Y - 1, (Y mod 4) =\= 0);
						(X-Y =:= 3, Z is Y - 3, ((Y-3) mod 4) =\= 0);
						(X-Y =:= 4, Z is Y - 4);
						(X-Y =:= 5, Z is Y - 5, (Y mod 4) =\= 0)));
						(var(Y) ->
						((Z-X =:= 2, Y is Z - 1, ((Y-3) mod 4) =\= 0);  % calcular Y se estiver em falta
						(Z-X =:= 6, Y is Z - 3, (Y mod 4) =\= 0);
						(Z-X =:= 8, Y is Z - 4);
						(Z-X =:= 10, Y is Z - 5, ((Y-3) mod 4) =\= 0);
						(X-Z =:= 2, Y is Z + 1, (Y mod 4) =\= 0);
						(X-Z =:= 6, Y is Z + 3, ((Y-3) mod 4) =\= 0);
						(X-Z =:= 8, Y is Z + 4);
						(X-Z =:= 10, Y is Z + 5, (Y mod 4) =\= 0))).



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

add_lists([], X, X).
add_lists([X | Y], Z, [X | W]) :- add_lists(Y, Z, W).

indexOf([Element|_], Element, 0).  % pode-se se trocar por nth0
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  Index is Index1+1.

% ite(I, T, _):- I, !, T.
% ite(_, _, E):- E.


% not(X):- X, !, fail.
% not(_X).

% ------------------- Valid Moves ------------------------------


% Moves sao os varios moves que todas as Pieces to tipo Piece podem realizar. Assume-se tamanho da board (4x4)

% piece_option([Map], RES) jogadas de place pieces possiveis (resultado é uma lista de ints que representam posições na board)

piece_option(Map, Moves) :- findall(I, nth0(I, Map, ' '), Moves).

% obter todas as jogadas possiveis ([init-end]) de move tendo em conta o player board(4x4)

move_option(Map, Piece, Moves) :- findall(Init-End, valid_move(Map, Piece, Init, End), Moves).

valid_move(Map, Piece, X, Y) :- indexOf(Map, ' ', Y), 
								indexOf(Map, Piece, X),
								((Y =:= X-1, X =\= 4, X=\=8, X=\=12);
								(Y =:= X+1, X=\=3, X=\=7, X=\=11);
								(Y =:= X+4);
								(Y =:= X-4);
								(Y =:= X-3, X=\=3, X=\=7, X=\=11, X=\=15); % /^
								(Y =:= X-5, X=\=4, X=\=8, X=\=12); % \^
								(Y =:= X+5, X=\=3, X=\=7, X=\=11); % \v 
								(Y =:= X+3, X=\=12, X=\=8, X=\=4, X=\=0)), % /v
								Y >= 0, Y =< 16.


% obter todas as jogadas possiveis ([init-end]) de eat tendo em conta o player board(4x4)
% eat_option(['x', 'o', ' ', ' ','o', 'o',' ', ' ',' ', 'o',' ', ' ',' ', ' ',' ', ' '],'x',X).
eat_option(Map, Piece, Moves) :- findall(Move, valid_eats(Map,Piece,Move, 15), [Moves|_]).


% eat_option([' ','x',' ',' ','o',' ',' ',' ',' ',' ',' ',' ',' ','x',' ',' '],'x',X).

% valid_eat(['x', 'o', ' ', ' ','o', 'o',' ', ' ',' ', 'o',' ', ' ',' ', ' ',' ', ' '],0,'x',2).
valid_eat(A,X,Piece,Z) :- 	(Y is X+1; Y is X-1; Y is X+3; Y is X-3; Y is X+4; Y is X-4; Y is X+5; Y is X-5),
							calc_eat_move(X, Y, Z), % calcular posiçao final
							what_piece(A, X, Piece),
							what_piece(A, Y, C),
							what_piece(A, Z, F),
							%(Z is X+2; Z is X-2; Z is X+6; Z is X-6; Z is X+8; Z is X-8; Z is X+10; Z is X-10),
							F == ' ',
							((Piece == 'x', C == 'o'); (Piece == 'o', C == 'x')).


valid_eats(A, Piece, Moves, 0) :- 	findall(0-Z, valid_eat(A,0,Piece,Z), Moves).
valid_eats(A, Piece, Moves, X) :- X > 0,
								findall(X-Z, valid_eat(A,X,Piece,Z), Move),
								add_lists(Move, Movs, Moves),
								Xs is X-1,
								valid_eats(A, Piece, Movs, Xs).

% --------------------------------------------------------------