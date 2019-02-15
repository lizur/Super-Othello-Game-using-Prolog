/****** alpha_beta_pruning ******/
alpha_beta_pruning(State, Depth, Color, NewState, Value):-
	alpha_beta_pruning(Depth, State, Color, NewState, Value, -1000, 1000).

alpha_beta_pruning(_, State, _, State, Value, _, _) :- final(State, Value),!.

alpha_beta_pruning(0, State, _, State, Value, _, _) :- eval(State, Value),!.

alpha_beta_pruning(Depth, State, Color, NewState, Value, Alpha, Beta) :-
	Depth > 0,
	garbage_collect,
	find_states(State, Color, StatesList),
	/*length(StatesList, L),
	writef('number of boards %d, depth %d\n', [L,Depth]),
	first_n_elements(7, BoardsList, NBoardsList),*/
	rival_color(Color, RivalColor),
	NDepth is Depth - 1,
	catch(
		alpha_beta_pruning(StatesList, NDepth, Color, RivalColor, NewState, Value, Alpha, Beta),
		_,
		alpha_beta_pruning_recover(StatesList, Depth, Color, RivalColor, NewState, Value, Alpha, Beta)).

alpha_beta_pruning_recover(StatesList, Depth, Color, RivalColor, NewState, Value, Alpha, Beta):-
	writef('recovered at depth %d\n', [Depth]),
	garbage_collect,
	alpha_beta_pruning(StatesList, 0, Color, RivalColor, NewState, Value, Alpha, Beta).

alpha_beta_pruning([State], Depth, _, RivalColor, State, Value, Alpha, Beta):- !,
	alpha_beta_pruning(Depth, State, RivalColor, _, Value, Alpha, Beta).

alpha_beta_pruning([State|Rest], Depth, Color, RivalColor, NewState, Value, Alpha, Beta) :-
	alpha_beta_pruning(Depth, State, RivalColor, _, X, Alpha, Beta),
	(
		prune(Color, X, Alpha, Beta) ->
		(
			NewState = State,
			Value is X
		);
		(
			recalc(Color, X, Alpha, Beta, Nalpha, NBeta),
			alpha_beta_pruning(Rest, Depth, Color, RivalColor, B, Y, Nalpha, NBeta),
			best(Color, X, Y, State, B, NewState, Value)
		)

	).
prune(black, Value, _, Beta):-
	Value >= Beta.
prune(white, Value, Alpha, _):-
	Value =< Alpha.

recalc(black, Value, Alpha, Beta, Nalpha, Beta):-
	max_list([Alpha, Value], Nalpha).
recalc(white, Value, Alpha, Beta, Alpha, NBeta):-
	min_list([Beta, Value], NBeta).
best(black, X, Y, A, _, A, X):- X>=Y,!.
best(black, _, Y, _, B, B, Y).
best(white, X, Y, A, _, A, X):- X=<Y, !.
best(white, _, Y, _, B, B, Y).

/******* end alpha_beta_pruning *******/

/******* board *******/


init_board(Board) :-
	Board = [[empty, empty, empty, empty, empty, empty, empty, empty],
		[empty, empty, empty, empty, empty, empty, empty, empty],
		[empty, empty, empty, empty, empty, empty, empty, empty],
		[empty, empty, empty, white, black, empty, empty, empty],
		[empty, empty, empty, black, white, empty, empty, empty],
		[empty, empty, empty, empty, empty, empty, empty, empty],
		[empty, empty, empty, empty, empty, empty, empty, empty],
		[empty, empty, empty, empty, empty, empty, empty, empty]].

print_board(Board) :-
	writeln('    0 1 2 3 4 5 6 7'),
	writeln('-------------------'),
	write('0| '),
	print_board(Board, 0, 0).

print_board(_, 7, 8):-
	writeln(''),
	writeln(''),!.

print_board(Board, RowIndex, 8) :-
	NRowIndex is RowIndex + 1,
	writef('\n%d| ', [NRowIndex]),
	print_board(Board, NRowIndex, 0).

print_board(Board, RowIndex, ColumnIndex) :-
	piece(Board, RowIndex, ColumnIndex, Piece),
	print_piece(Piece),
	NColumnIndex is ColumnIndex + 1,
	print_board(Board, RowIndex, NColumnIndex).

print_piece(black):-
	write(' X').

print_piece(white):-
	write(' 0').

print_piece(empty):-
	write(' -').

piece(Board, RowIndex, ColumnIndex, Piece) :-
	is_valid_index(RowIndex),
	is_valid_index(ColumnIndex),
	nth0(RowIndex, Board, Row),
	nth0(ColumnIndex, Row, Piece).

final(Board, Value):-
	full_board(Board),
	count_pieces(black, Board, BlackPieces, WhitePieces),
	Value is BlackPieces - WhitePieces.

eval(Board, Value):-
	count_pieces(black, Board, BlackPieces, WhitePieces),
	HeuristicValue1 is BlackPieces - WhitePieces,
	valid_positions(Board, black, BlackValidMoves),
	valid_positions(Board, white, WhiteValidMoves),
	HeuristicValue2 is BlackValidMoves - WhiteValidMoves,
	max_list([HeuristicValue1,HeuristicValue2], Value).

empty_on_board(Board):-
	member(Row, Board),
	member(Piece, Row),
	Piece = empty,!.

full_board(Board):-
	flatten(Board, PiecesList),
	list_to_set(PiecesList, PiecesSet),
	not(member(empty, PiecesSet)).

full_board(Board, Color, Value):-
	flatten(Board, PiecesList),
	list_to_set(PiecesList, PiecesSet),
	not(member(empty, PiecesSet)),
	count_pieces(Color, Board, Pieces, RivalPieces),
	Value is Pieces - RivalPieces.

find_states(State, Color, StatesList):-
	find_boards(State, Color, StatesList).

find_boards(Board, Color, BoardsList):-
	find_moves(Board, Color, MovesList),
	find_boards(Board, Color, OrderedBoardsList, [], MovesList),
	first_elements(OrderedBoardsList, [], BoardsList).

find_boards(Board,_, BoardsList, [], []):-
	append([], [[Board, 0]], BoardsList),!.

find_boards(_, _, BoardsList, BoardsList, []):-!.

find_boards(Board, Color, BoardsList, CurrentBoardsList, [Move|RestMovesList]):-
	set_piece(Board, Move, Color, FinalBoard),
	order_boards(Color, CurrentBoardsList, FinalBoard, NBoardsList),
	find_boards(Board, Color, BoardsList, NBoardsList, RestMovesList),!.

order_boards(Color, CurrentBoardsList, FinalBoard, NBoardsList):-
	rival_color(Color, RivalColor),
	valid_positions(FinalBoard, RivalColor, Number),
	order_boards_aux([FinalBoard, Number], CurrentBoardsList, [], NBoardsList).

order_boards_aux(Board, [], CurrentList, FinalList):-
	append(CurrentList, [Board], FinalList),!.

order_boards_aux(Board, [First|Rest], CurrentList, FinalList):-
	nth0(1, First, Value),
	nth0(1, Board, NewValue),
	NewValue =< Value,
	append(CurrentList, [Board], TempList),
	append(TempList, [First|Rest], FinalList),!.

order_boards_aux(Board, [First|Rest], CurrentList, FinalList):-
	append(CurrentList, [First], NCurrentList),
	order_boards_aux(Board, Rest, NCurrentList, FinalList),!.

valid_positions(Board, Color, Number):-
	valid_positions(Board, Color, 0, 0, 0, Number).

valid_positions(_, _, 7, 8, Number, Number):-!.

valid_positions(Board, Color, RowIndex, 8, CurrentNumber, FinalNumber):-
	NRowIndex is RowIndex + 1,
	valid_positions(Board, Color, NRowIndex, 0, CurrentNumber, FinalNumber),!.

valid_positions(Board, Color, RowIndex, ColumnIndex, CurrentNumber, FinalNumber):-
	single_valid_move(Board, RowIndex, ColumnIndex, Color),
	NCurrentNumber is CurrentNumber + 1,
	NColumnIndex is ColumnIndex + 1,
	valid_positions(Board, Color, RowIndex, NColumnIndex, NCurrentNumber, FinalNumber),!.

valid_positions(Board, Color, RowIndex, ColumnIndex, CurrentNumber, FinalNumber):-
	NColumnIndex is ColumnIndex + 1,
	valid_positions(Board, Color, RowIndex, NColumnIndex, CurrentNumber, FinalNumber),!.

single_valid_move(Board, RowIndex, ColumnIndex, Color) :-
	piece(Board, RowIndex, ColumnIndex, Piece),
	Piece = empty,
	direction_offsets(DirectionOffsets),
	member(DirectionOffset, DirectionOffsets),
	nth0(0, DirectionOffset, RowOffset),
	nth0(1, DirectionOffset, ColumnOffset),
	NeighborRow is RowIndex + RowOffset,
	NeighborColumn is ColumnIndex + ColumnOffset,
	rival_color(Color, RivalColor),
	piece(Board, NeighborRow, NeighborColumn, NeighborPiece),
	NeighborPiece = RivalColor,
	find_color(Board, NeighborRow, NeighborColumn, RowOffset, ColumnOffset, Color),!.

find_moves(Board, Color, MovesList):-
	find_moves(Board, Color, 0, 0, [], MovesList).

find_moves(_, _, 7, 8, MovesList, MovesList):-!.

find_moves(Board, Color, RowIndex, 8, MovesList, FinalList):-
	NRowIndex is RowIndex + 1,
	find_moves(Board, Color, NRowIndex, 0, MovesList, FinalList),!.

find_moves(Board, Color, RowIndex, ColumnIndex, MovesList, FinalList):-
	valid_move(Board, RowIndex, ColumnIndex, Color, ValidDirectionOffsets),
	append(MovesList,[[RowIndex, ColumnIndex, ValidDirectionOffsets]], NMovesList),
	NColumnIndex is ColumnIndex + 1,
	find_moves(Board, Color, RowIndex, NColumnIndex, NMovesList, FinalList),!.

find_moves(Board, Color, RowIndex, ColumnIndex, MovesList, FinalList):-
	NColumnIndex is ColumnIndex + 1,
	find_moves(Board, Color, RowIndex, NColumnIndex, MovesList, FinalList),!.

valid_move(Board, RowIndex, ColumnIndex, Color, ValidDirectionOffsets):-
	piece(Board, RowIndex, ColumnIndex, Piece),
	Piece = empty,
	direction_offsets(DirectionOffsets),
	valid_move(Board, RowIndex, ColumnIndex, Color, DirectionOffsets, [], ValidDirectionOffsets).

valid_move(_, _, _, _, [], CurrentValidDirectionOffsets, ValidDirectionOffsets):-
	CurrentValidDirectionOffsets \= [],
	CurrentValidDirectionOffsets = ValidDirectionOffsets.

valid_move(Board, RowIndex, ColumnIndex, Color, DirectionOffsets, CurrentValidDirectionOffsets, ValidDirectionOffsets):-
	DirectionOffsets = [DirectionOffset|DirectionOffsetsRest],
	valid_move_offset(Board, RowIndex, ColumnIndex, Color, DirectionOffset),
	append(CurrentValidDirectionOffsets, [DirectionOffset], NCurrentValidDirectionOffsets),
	valid_move(Board, RowIndex, ColumnIndex, Color, DirectionOffsetsRest, NCurrentValidDirectionOffsets, ValidDirectionOffsets).

valid_move(Board, RowIndex, ColumnIndex, Color, DirectionOffsets, CurrentValidDirectionOffsets, ValidDirectionOffsets):-
	DirectionOffsets = [_|DirectionOffsetsRest],
	valid_move(Board, RowIndex, ColumnIndex, Color, DirectionOffsetsRest, CurrentValidDirectionOffsets, ValidDirectionOffsets).

valid_move_offset(Board, RowIndex, ColumnIndex, Color, DirectionOffset):-
	piece(Board, RowIndex, ColumnIndex, Piece),
	Piece = empty,
	nth0(0, DirectionOffset, RowOffset),
	nth0(1, DirectionOffset, ColumnOffset),
	NeighborRow is RowIndex + RowOffset,
	NeighborColumn is ColumnIndex + ColumnOffset,
	rival_color(Color, RivalColor),
	piece(Board, NeighborRow, NeighborColumn, NeighborPiece),
	NeighborPiece = RivalColor,
	find_color(Board, NeighborRow, NeighborColumn, RowOffset, ColumnOffset, Color).

find_color(Board, RowIndex, ColumnIndex, RowOffset, ColumnOffset, Color) :-
	NRowOffset is RowIndex + RowOffset,
	NColumnOffset is ColumnIndex + ColumnOffset,
	piece(Board, NRowOffset, NColumnOffset, Piece),
	Piece = Color.

find_color(Board, RowIndex, ColumnIndex, RowOffset, ColumnOffset, Color) :-
	NRowIndex is RowIndex + RowOffset,
	NColumnIndex is ColumnIndex + ColumnOffset,
	piece(Board, NRowIndex, NColumnIndex, Piece),
	rival_color(Color, RivalColor),
	Piece = RivalColor,
	find_color(Board, NRowIndex, NColumnIndex, RowOffset, ColumnOffset, Color).

set_piece(Board, Move, Color, FinalBoard):-
	nth0(0, Move, Row),
	nth0(1, Move, Column),
	nth0(2, Move, ValidDirectionOffsets),
	set_single_piece(Board, Row, Column, Color, BoardWithPiece),
	set_pieces_on_offsets(BoardWithPiece, Row, Column, Color, ValidDirectionOffsets, FinalBoard).

set_piece(Board, PieceRowIndex, PieceColumnIndex, Color, FinalBoard):-
	valid_move(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffsets),
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, Color, BoardWithPiece),
	set_pieces_on_offsets(BoardWithPiece, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffsets, FinalBoard).

set_pieces_on_offsets(FinalBoard, _, _, _, [], FinalBoard):-!.

set_pieces_on_offsets(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffsets, FinalBoard):-
	ValidDirectionOffsets = [ValidDirectionOffset|ValidDirectionOffsetsRest],
	set_pieces_on_offset(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffset, TempBoard),
	set_pieces_on_offsets(TempBoard, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffsetsRest, FinalBoard).

set_pieces_on_offset(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffset, FinalBoard):-
	nth0(0, ValidDirectionOffset, RowOffset),
	nth0(1, ValidDirectionOffset, ColumnOffset),
	NRowOffset is PieceRowIndex + RowOffset,
	NColumnOffset is PieceColumnIndex + ColumnOffset,
	piece(Board, NRowOffset, NColumnOffset, Piece),
	Piece = Color,
	Board = FinalBoard,!.

set_pieces_on_offset(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffset, FinalBoard):-
	nth0(0, ValidDirectionOffset, RowOffset),
	nth0(1, ValidDirectionOffset, ColumnOffset),
	NRowOffset is PieceRowIndex + RowOffset,
	NColumnOffset is PieceColumnIndex + ColumnOffset,
	piece(Board, NRowOffset, NColumnOffset, Piece),
	rival_color(Color, RivalColor),
	Piece = RivalColor,
	set_single_piece(Board, NRowOffset, NColumnOffset, Color, TempBoard),
	set_pieces_on_offset(TempBoard, NRowOffset, NColumnOffset, Color, ValidDirectionOffset, FinalBoard).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, Color, FinalBoard):-
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, 0, 0, Color, [], FinalBoard, []).

set_single_piece(_, 7, _, 7, 8, _, ResultingBoard, FinalBoard, PieceRow):-
	append(ResultingBoard, [PieceRow], FinalBoard),!.

set_single_piece(_, _, _, 8, 0, _, FinalBoard, FinalBoard, _):-!.

set_single_piece(Board, PieceRowIndex, ColumnRowIndex, PieceRowIndex, 8, Color, ResultingBoard, FinalBoard, RowIndex):-
	PieceRowIndex \= 7,
	NCurrentRowIndex is PieceRowIndex + 1,
	append(ResultingBoard, [RowIndex], NResultingBoard),
	set_single_piece(Board, PieceRowIndex, ColumnRowIndex, NCurrentRowIndex, 0, Color, NResultingBoard, FinalBoard, []).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, PieceColumnIndex, Color, ResultingBoard, FinalBoard, PieceRow):-
	append(PieceRow, [Color], NPieceRow),
	NCurrentColumnIndex is PieceColumnIndex + 1,
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, NCurrentColumnIndex, Color, ResultingBoard, FinalBoard, NPieceRow).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, CurrentColumnIndex, Color, ResultingBoard, FinalBoard, PieceRow):-
	CurrentColumnIndex \= PieceColumnIndex,
	piece(Board, PieceRowIndex, CurrentColumnIndex, Piece),
	append(PieceRow, [Piece], NPieceRow),
	NCurrentColumnIndex is CurrentColumnIndex + 1,
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, NCurrentColumnIndex, Color, ResultingBoard, FinalBoard, NPieceRow).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, CurrentRowIndex, _, Color, ResultingBoard, FinalBoard, PieceRow):-
	PieceRowIndex \= CurrentRowIndex,
	nth0(CurrentRowIndex, Board, CurrentRow),
	append(ResultingBoard, [CurrentRow], NResultingBoard),
	NCurrentRowIndex is CurrentRowIndex + 1,
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, NCurrentRowIndex, 0, Color, NResultingBoard, FinalBoard, PieceRow).

count_pieces(Color, Board, Pieces, RivalPieces) :-
	count_pieces(Color, Board, 0, 0, 0, 0, Pieces, RivalPieces).

count_pieces(_, _, 7, 8, CurrentPieces, CurrentRivalPieces, Pieces, RivalPieces):-
	Pieces is CurrentPieces,
	RivalPieces is CurrentRivalPieces,!.

count_pieces(Color, Board, RowIndex, 8, CurrentPieces, CurrentRivalPieces, Pieces, RivalPieces) :-
	NRowIndex is RowIndex + 1,
	count_pieces(Color, Board, NRowIndex, 0, CurrentPieces, CurrentRivalPieces, Pieces, RivalPieces).

count_pieces(Color, Board, RowIndex, ColumnIndex, CurrentPieces, CurrentRivalPieces, Pieces, RivalPieces) :-
	piece(Board, RowIndex, ColumnIndex, Piece),
	count_piece(Color, Piece, CurrentPieces, CurrentRivalPieces, NCurrentPieces, NCurrentRivalPieces),
	NColumnIndex is ColumnIndex + 1,
	count_pieces(Color, Board, RowIndex, NColumnIndex, NCurrentPieces, NCurrentRivalPieces, Pieces, RivalPieces).

count_piece(_, empty, CurrentPieces, CurrentRivalPieces, CurrentPieces, CurrentRivalPieces):-!.

count_piece(Color, Color, CurrentPieces, CurrentRivalPieces, NCurrentPieces, CurrentRivalPieces):-
	NCurrentPieces is CurrentPieces + 1,!.

count_piece(Color, RivalColor, CurrentPieces, CurrentRivalPieces, CurrentPieces, NCurrentRivalPieces):-
	rival_color(Color, RivalColor),
	NCurrentRivalPieces is CurrentRivalPieces + 1,!.

direction_offsets(OffsetsList) :-
	OffsetsList = [[-1, 0],
			[-1, 1],
			[0, 1],
			[1, 1],
			[1, 0],
			[1, -1],
			[0, -1],
			[-1,-1]].

is_valid_index(Index) :-
	Index >= 0,
	Index < 8.

/******** end board ********/


/******* game ********/


play(Depth) :-
	init_board(Board),
	select_mode(Mode),
	game_loop(Board, Mode, Depth, black).

game_loop(Board, 1, Depth, black):-
	print_board(Board),
	print_player(black),
	empty_on_board(Board),
	find_moves(Board, black, MovesList),
	member(_, MovesList),
	human_select_move(Move, MovesList),!,
	set_piece(Board, Move, black, FinalBoard),
	game_loop(FinalBoard, 1, Depth, white),!.

game_loop(Board, 1, Depth, white):-
	print_board(Board),
	print_player(white),
	empty_on_board(Board),
	machine_select_move(Board, Depth, white, FinalBoard),!,
	game_loop(FinalBoard, 1, Depth, black),!.

game_loop(Board, 2, Depth, black):-
	print_board(Board),
	print_player(black),
	empty_on_board(Board),
	machine_select_move(Board, Depth, black, FinalBoard),!,
	game_loop(FinalBoard, 2, Depth, white),!.

game_loop(Board, 2, Depth, white):-
	print_board(Board),
	print_player(white),
	empty_on_board(Board),
	find_moves(Board, white, MovesList),
	member(_, MovesList),
	human_select_move(Move, MovesList),!,
	set_piece(Board, Move, white, FinalBoard),
	game_loop(FinalBoard, 2, Depth, black),!.


game_loop(Board, 3, Depth, Color):-
	print_board(Board),
	print_player(Color),
	empty_on_board(Board),
	find_moves(Board, Color, MovesList),
	member(_, MovesList),
	human_select_move(Move, MovesList),!,
	set_piece(Board, Move, Color, FinalBoard),
	rival_color(Color, RivalColor),
	game_loop(FinalBoard, 3, Depth, RivalColor),!.

game_loop(Board, _, _, Color):-
	full_board(Board),
	print_board(Board),
	count_pieces(Color, Board, Pieces, RivalPieces),
	writef('%d: %d\n', [Color, Pieces]),
	rival_color(Color, RivalColor),
	writef('%d: %d\n', [RivalColor, RivalPieces]),!.

game_loop(Board, Mode, Depth, Color):-
	find_moves(Board, Color, MovesList),!,
	not(member(_,MovesList)),!,
	print_player(Color),
	writeln('There\'s no valid move'),
	rival_color(Color, RivalColor),
	game_loop(Board, Mode, Depth, RivalColor),!.

print_player(white):-
	writeln('White player turn (0)'),!.

print_player(black):-
	writeln('Black player turn (X)'),!.

human_select_move(Move, MovesList):-
	write('Enter the Row: '),
	read(SelectedRow),
	writeln('Enter the Column: '),
	read(SelectedColum),
	member(Move, MovesList),
	nth0(0, Move, SelectedRow),
	nth0(1, Move, SelectedColum).


human_select_move(Move, MovesList):-
	writeln('Not a valid move'),
	writeln(''),
	human_select_move(Move, MovesList).

machine_select_move(Board, Depth, Color, FinalBoard):-
	garbage_collect,
	alpha_beta_pruning(Board, Depth, Color, FinalBoard, _).


select_mode(Mode):-
	writeln('Select a game mode'),
	writeln('1. human vrs machine'),
	writeln('2. machine vrs human'),
	writeln('3. human vrs human'),
	write('Enter a number: '),
	read(SelectedMode),
	enter_mode(SelectedMode, Mode).

enter_mode(SelectedMode, Mode):-
	SelectedMode is 1,
	Mode is SelectedMode,
	writeln('machine vrs human selected'),
	writeln(''),!.
enter_mode(SelectedMode, Mode):-
	SelectedMode is 2,
	Mode is SelectedMode,
	writeln('human vrs machine selected'),
	writeln(''),!.
enter_mode(SelectedMode, Mode):-
	SelectedMode is 3,
	Mode is SelectedMode,
	writeln('human vrs human selected'),
	writeln(''),!.
enter_mode(_, Mode):-
	writeln('Not a valid mode'),
	writeln(''),
	select_mode(Mode).

rival_color(white, black).
rival_color(black, white).


/********** end game **********/

/********** utilities **********/


first_elements([], BoardsList, BoardsList):-!.

first_elements([First|Rest], Temp, Boards):-
	nth0(0, First, Board),
	append(Temp, [Board], NTemp),
	first_elements(Rest, NTemp, Boards).

first_n_elements(Number, List, NList):-
		length(List, N),
		N =< Number,
		List = NList,!.

first_n_elements(Number, List, NList):-
	first_n_elements_aux(Number, List, [], NList).

first_n_elements_aux(0, _, NList, NList):-!.

first_n_elements_aux(Number, [First|Rest], TempList, NList):-
	NNumber is Number - 1,
	append(TempList, [First], NTempList),
	first_n_elements_aux(NNumber, Rest, NTempList, NList).


min_list([First|Rest], Min):-
	min_list_aux(Rest, First, Min).

min_list_aux([], Min, Min):-!.

min_list_aux([First|Rest], CurrentMin, Min):-
	First < CurrentMin,
	min_list_aux(Rest, First, Min),!.

min_list_aux([_|Rest], CurrentMin, Min):-
	min_list_aux(Rest, CurrentMin, Min),!.

max_list([First|Rest], Max):-
	max_list_aux(Rest, First, Max).

max_list_aux([], Max, Max):-!.

max_list_aux([First|Rest], CurrentMax, Max):-
	First > CurrentMax,
	max_list_aux(Rest, First, Max),!.

max_list_aux([_|Rest], CurrentMax, Max):-
	max_list_aux(Rest, CurrentMax, Max),!.

/********** end utilities **********/
