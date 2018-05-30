%PREV
pert(X,[X|_]).
pert(X,[_|L]):-pert(X,L).
concat([],L,L).
concat([X|L1],L2,[X|L3]):-concat(L1,L2,L3).
pert_con_resto(X,L,R):-concat(L1,[X|L2],L),concat(L1,L2,R).

%NOW
%1. Flatten aplana lista[a,[b,c,[b,b],e],f] => [a,b,c,b,b,e,f]
	%AUX: es_lista dice si X es lista

es_lista([]).
es_lista([_|_]).
flatten([],[]).
flatten([X|L], L2):- es_lista(X),!,flatten(X,X2), flatten(L,L1), concat(X2,L1,L2).
flatten([X|L],[X|L2]):- flatten(L,L2).

%Se puede aplanar directamente con llamada a flaten recursiva?
%flatten([],[]):-!.
%flatten([X|L], L2):- !,flatten(X,X2), flatten(L,L1), concat(X2,L1,L2).
%flatten(X,[X]).

%1.b Flatten+sinreps
noRepeticions([],[]).
noRepeticions([X|L],[X|L2]):- pert_con_resto(X,L,R),!, noRepeticions([X|R],[X|L2]).
noRepeticions([X|L],[X|L2]):- noRepeticions(L,L2).
flattenNoRepetitions(L,L2):- flatten(L,L1), noRepeticions(L1,L2).



%2. Soluciona:
%    1 - El que vive en la casa roja es de Peru
%    2 - Al frances le gusta el perro
%    3 - El pintor es japones
%    4 - Al chino le gusta el ron
%    5 - El hungaro vive en la primera casa
%    6 - Al de la casa verde le gusta el coñac
%    7 - La casa verde esta a la izquierda de la blanca
%    8 - El escultor cría caracoles
%    9 - El de la casa amarilla es actor
%   10 - El de la tercera casa bebe cava
%   11 - El que vive al lado del actor tiene un caballo
%   12 - El hungaro vive al lado de la casa azul
%   13 - Al notario la gusta el whisky
%   14 - El que vive al lado del medico tiene un ardilla,


casas:- Sol = [	[1,_,_,_,_,_], % casa,color,profesion,animal,bebida,pais.
		[2,_,_,_,_,_],
		[3,_,_,_,_,_],
		[4,_,_,_,_,_],
		[5,_,_,_,_,_]],
	member([_,rojo	,_	,_	,_	,peru	],Sol), %1
	member([_,_	,_	,perro	,_	,francia],Sol),%2
	member([_,_	,pintor	,_	,_	,japon	],Sol),%3
	member([_,_	,_	,_	,ron	,china	],Sol), %4
	member([1,_	,_	,_	,_	,hungria],Sol),%5
	member([_,verde	,_	,_	,coñac	,_	],Sol),%6

	member([A,verde	,_	,_	,_	,_	],Sol), %7
	A1 is A+1,
	member([A1,blanca,_	,_	,_	,_	],Sol),

	member([_,_	,escultor,caracol,_	,_	],Sol),%8
	member([_,amarillo,actor,_	,_	,_	],Sol),%9
	member([3,_	,_	,_	,cava	,_	],Sol),%10

	member([B,_	,actor	,_	,_	,_	],Sol),%11
	B1 is B-1,
	B2 is B+1,
	member(B3,[B1,B2]),
	member([B3,_	,_	,caballo,_	,_	],Sol),

	member([C,azul	,_	,_	,_	,_	],Sol),%12
	C1 is C-1,
	C2 is C+1,
	member(C3,[C1,C2]),
	member([C3,_	,_	,_	,_	,hungria],Sol),

	member([_,_	,notario,_	,whisky	,_	],Sol), %13

	member([D,_	,medico	,_	,_	,_	],Sol), %14 !!!!
	D1 is D-1,
	D2 is D+1,
	member(D3,[D1,D2]),
	member([D3,_	,_	,ardilla,_	,_	],Sol),
	write(Sol),nl.

%3. Reinas en 8x8.
% llista de parelles?
iessim([J|_],1,J).
iessim([_,A|L],I,X):-I>1, I1 is I-1, iessim([A|L],I1,X). %A perque necesito 2 elems

noamenaçat(_,0,_):-!.
noamenaçat(NEW, N, Sol):- % diu si les reines a sol de 1 a n no amenacen a la nova reina
	iessim(Sol, N, Q),
	noamenaça(NEW,Q),
	N1 is N-1, noamenaçat(NEW,N1,Sol).

noamenaça([I1,J1],[I2,J2]):-
	J1\=J2,
	D11 is I1+J1, D12 is I2+J2,
	D11 \= D12,
	D21 is I1-J1, D22 is I2-J2,
	D21 \= D22.

reinas:- Sol=[[1,J1],[2,J2],[3,J3],[4,J4],[5,J5],[6,J6],[7,J7],[8,J8]],
	between(1,8,J1),between(1,8,J2),between(1,8,J3),between(1,8,J4),
	between(1,8,J5),between(1,8,J6),between(1,8,J7),between(1,8,J8),
	noamenaçat([2,J2],1,Sol),
	noamenaçat([3,J3],2,Sol),
	noamenaçat([4,J4],3,Sol),
	noamenaçat([5,J5],4,Sol),
	noamenaçat([6,J6],5,Sol),
	noamenaçat([7,J7],6,Sol),
	noamenaçat([8,J8],7,Sol), writeBoard(Sol,1,1),nl.%, fail.
%reinas.
writeBoard(_,9,_):-!.
writeBoard(Sol,I,J):- pert([I,J], Sol),!, write("X "), advance(I,J,I1,J1), writeBoard(Sol, I1,J1).
writeBoard(Sol,I,J):- write(". "), advance(I,J,I1,J1), writeBoard(Sol, I1,J1).

advance(I, J, I, J1):- J1 is J + 1, J1 < 9, !.
advance(I, _, I1, 1):- I1 is I + 1,nl.

natList(0,[]).
natList(N,[N|C]):- N1 is N-1, natList(N1,C).

permutacion([],[]).
permutacion(L,[X|P]) :- pert_con_resto(X,L,R), permutacion(R,P).

permutacionNat(0, []):-!.
permutacionNat(N, S):- natList(N, X), permutacion(X,S).

no_pert(X,L):- member(X,L), !, fail.
no_pert(_,_).

no_amenaça2([],[],_,_).
no_amenaça2([I|F], [J|C], DA,DB):-
	DIA is I+J, DIB is I-J,
	no_pert(DIA, DA), no_pert(DIB,DB),
	concat([DIA],DA, DA1), concat([DIB],DB,DB1),
	no_amenaça2(F,C,DA1,DB1).
mergeLists([],[],[]).
mergeLists([X|A],[Y|B], [[X,Y]|C]):- mergeLists(A,B,C).
%BUENA!!
reinas2:- permutacionNat(8, FILAS),permutacionNat(8, COLS),
	no_amenaça2(FILAS,COLS,[],[]), mergeLists(FILAS,COLS, SOL),writeBoard(SOL, 1, 1), nl.



