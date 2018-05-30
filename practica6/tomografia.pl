% A matrix which contains zeroes and ones gets "x-rayed" vertically and
% horizontally, giving the total number of ones in each row and column.
% The problem is to reconstruct the contents of the matrix from this
% information. Sample run:
%
%	?- p.
%	    0 0 7 1 6 3 4 5 2 7 0 0
%	 0                         
%	 0                         
%	 8      * * * * * * * *    
%	 2      *             *    
%	 6      *   * * * *   *    
%	 4      *   *     *   *    
%	 5      *   *   * *   *    
%	 3      *   *         *    
%	 7      *   * * * * * *    
%	 0                         
%	 0                         
%	

:- use_module(library(clpfd)).

ejemplo1( [0,0,8,2,6,4,5,3,7,0,0], [0,0,7,1,6,3,4,5,2,7,0,0] ).
ejemplo2( [10,4,8,5,6], [5,3,4,0,5,0,5,2,2,0,1,5,1] ).
ejemplo3( [11,5,4], [3,2,3,1,1,1,1,2,3,2,1] ).


p:-	ejemplo3(RowSums,ColSums),
	length(RowSums,NumRows),
	length(ColSums,NumCols),
	NVars is NumRows*NumCols,
	length(L,NVars),  % generate a list of Prolog vars (their names do not matter) !!!might cause problems!!!! (use length!)
	
	L ins 0..1,

	matrixByRows(L,NumCols,MatrixByRows), % convertir lista en lista de lista por lineas
	transpose(MatrixByRows, MatrixByCols), %transponer para restricciones verticales
	
	sumConstraint(MatrixByRows, RowSums),
	sumConstraint(MatrixByCols, ColSums),
	
	
	pretty_print(RowSums,ColSums,MatrixByRows).

matrixByRows([],_,[]):-!.
matrixByRows(L, NumCols, [X|Q]):-
	pick(L, NumCols, X, R),
	matrixByRows(R, NumCols, Q).

pick(L, 0, [], L):-!. 
pick([X|L], N, [X|Q], R):- %put N elems from 1st to 3rd and leave rest in Q
	N1 #= N-1,
	pick(L, N1, Q, R).

sumConstraint([],[]).
sumConstraint([X|L1], [S|L2]):-
	lsum(X, S), 
	sumConstraint(L1,L2).
lsum([],0).
lsum([X|L], S):-
	lsum(L,S1),
	S #= S1+X.


pretty_print(_,ColSums,_):- write('     '), member(S,ColSums), writef('%2r ',[S]), fail.
pretty_print(RowSums,_,M):- nl,nth1(N,M,Row), nth1(N,RowSums,S), nl, writef('%3r   ',[S]), member(B,Row), wbit(B), fail.
pretty_print(_,_,_).
wbit(1):- write('*  '),!.
wbit(0):- write('   '),!.
    
