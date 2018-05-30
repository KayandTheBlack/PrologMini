% suma(L,S), donada L una llista d'enters, S es la seva Suma'
suma([],0).
suma([X|L],S):- suma(L,S1), S is S1+X. %recursive def
% pert(X,L). Donada L llista X elem, X es element de la llista. Sota BT, tinc tots els X posibles. (pert(X,[1,4,2]) retorna X=1, X=4, X=2
% no cal cas [] perque ja es fals.
pert(X,[X|_]). % cas 0
pert(X,[_|L]):-pert(X,L).
% Concat(L1,L2,L3) concatena L1 con L2 en L3
concat([],L,L).
concat([X|L1],L2,[X|L3]):-concat(L1,L2,L3).
% pert_con_resto(X,L,R); retorna X element que pertany i R la resta
% L es[1,2,3] llavors X=1 R = [2,3], X=2 R=[1,3]...
% Idea: partir L en 2.
pert_con_resto(X,L,R):-concat(L1,[X|L2],L),concat(L1,L2,R).

subcjto([],[]). %subcjto(L,S) es: "S es un subconjunto de L".
subcjto([X|C],[X|S]):-subcjto(C,S).
subcjto([_|C],S):-subcjto(C,S).


%1. Demostrar por inducción la correctitud de pert-con-resto, long, permutacion y subconjunto.
%a. pert_con_resto base: tenemos el elemento X, que forma parte de L por definición, siendo L de longitud 1. L1 y L2 están vacías, y por ello su concatenación R tambien lo está. (caso base correcto).
%   inducción: Supongamos que pertenece con resto funciona para una lista de longitud N. Entonces, en una lista de longitud N+1, tendremos que el nuevo elemento está en L1 o L2, con la posibilidad de que sea X. Si está en L1, pert_con_resto sencillamente extraerá X si ya estaba, por inducción, y el Resto contendrá el nuevo elemento de L1. Análogamente para L2. Si el elemento es X, sabemos que la lista se puede partir en dos respecto a ese punto, dando como resto la concatenación de sus dos lados, y en caso de que ya hubiera una X, por hipótesis, sabemos que tambien se puede partir respecto a la otra X, dejando la nueva en uno de los dos lados.

%b. long base: Si la lista está vacía, devuelve 0 por definición
% 	inducción: Supongamos que long devuelve longitudes correctas hasta listas de longitud n. Entonces, la lista de longitud n+1 debe tener 1+ longitud de una lista de longitud n. Por HI, podemos calcular esto con long.

%c. permutación base: La permutación de vacío es vacío por definición.
% 	inducción: permutación permuta listas de n elementos. Hacer una permutación con n+1 elementos, tan solo tenemos que hacer la permutación de n elementos cualesquiera de la lista inicial, y colocar el elemento sobrante en la permutación.

%d. subcjt base: vacío es subconjunto de vacío.
% 	inducción: supongamos que subcjt nos da ello para listas S de longitud n, y C de longitud >= n. Añadimos un nuevo elemento a S. Si dicho elemento no está en C, no entramos en la clausula de [X|C], [X|S], y en consecuencia no disminuiremos la longitud de la lista, no llegando nunca al caso base (efectivamente denotando que S no es subconjunto). En caso de que sí que lo esté, si S-X es subconjunto de C-X (por HI), dará una respuesta correcta. (NOTESE que las listas deben estar ordenadas para que el programa funcione, o la tercera cláusula podría salterse elementos de C que sí que estaban en S, sin quitarlos).
%2.prod(L,P) P es producto de los elementos de L.
prod([],1).
prod([X|L],P):- prod(L,P2), P is P2*X.
%3.pescalar(L1,L2,P) P es producto escalar de L1, L2. Falla si longitud diferente.
pescalar([],[],0).
pescalar([X|L1],[Y|L2],P):- pescalar(L1,L2,P1), P is P1+X*Y.
%4.Unió+Intersección sin repetición
% unió([1,3,2],[2,3,4],[1,3,2,4]).
unio([],L,L).
unio([X|L1],L2,U):-pert(X,L2),!, unio(L1,L2,U).
unio([X|L1],L2,[X|U]):-unio(L1,L2,U).
inters([],_,[]).
inters([X|L1],L2,[X|I]):-pert(X,L2),!,inters(L1,L2,I).
inters([_|L1],L2,I):-inters(L1,L2,I).
%5. ultim(L, X) X es ultimo elemento de L.
%ultim([X], X).
%ultim([_|L], X):-ultim(L,X).
ultim(L,X):- concat(_,[X],L),!. %! para la evaluación
%5. invert(L1,L2) L2 es la invertida de L1
invert([],[]).
invert([X|L1],L2):- invert(L1,L3), concat(L3,[X],L2).
%6. fib(N,F)
fib(1,1):-!.
fib(2,1):-!.
fib(N,F):- N1 is N-1, N2 is N-2, fib(N1,F1), fib(N2,F2), F is F1+F2.
%7 Dados(P,N,L) L es modo de sumar P con N dados.
dados(0,0,[]).
dados(P,N,[X|L]):- N > 0, pert(X,[1,2,3,4,5,6]), P1 is P-X, N1 is N-1, dados(P1,N1,L).
%8 suma_demas(L) exists X st. X = suma de L-X
suma_demas(L):-pert_con_resto(X,L,R), suma(R,X),!.
%9 suma_ants(L) exists X es suma de antecesores
suma_ants(L):-concat(L1,[X|_],L),suma(L1,X),!.
%10 card(L) escribe [[elem, aparicions],...]
card(L):-cardinalidad(L,RES),write(RES),!.
cardinalidad([],[]).
cardinalidad([X|L],RES):-cardinalidad(L,RES1),pert_con_resto([X,N1],RES1,R),!, N is N1+1, concat([[X,N]],R,RES).
cardinalidad([X|L],[[X,1]|RES]):-cardinalidad(L,RES).
%11 está_ordenada(L).
esta_ordenada([_]):-!.
esta_ordenada([X,X1|L]):- X=<X1, esta_ordenada([X1|L]).
%12 ordenación(L1,L2) L2 es L1 ordenada.
permutacion([],[]).
permutacion(L,[X|P]) :- pert_con_resto(X,L,R), permutacion(R,P).
ordenacion(L1,L2):- permutacion(L1,L2),esta_ordenada(L2),!.
%13 Intentará hacer todas las permutaciones, & ultima es correcta -> n! repeticiones con n-1 comparaciones cada una (n-1)*n! ops
%14 ord2
% insercion(X,L1,L2) inserta X en L1, resultado en L2.
ord2([],[]).
ord2([X|L1],L2):-ord2(L1,R),insercion(X,R,L2).
insercion(X,[],[X]).
insercion(X,[X1|L],[X,X1|L]):-X =< X1,!.
insercion(X,[X1|L],[X1|O]):-insercion(X,L,O).
%15: n^2
%16 mergsort(L1,L2)
long([],0).
long([_|L],M):- long(L,N), M is N+1.
mergsort(L1,L1):- long(L1,1),!.
mergsort(L1,L2):- 
	long(L1,N), N1 is N//2, concat(L11,L12,L1), long(L11,N1),
	mergsort(L11,R1), mergsort(L12,R2),
	fusion(R1,R2,L2),!.
fusion([],L,L):-!.
fusion(L,[],L):-!.
fusion([X1|L1],[X2|L2], [X1|R]):- X1<X2,fusion(L1,[X2|L2],R),!.
fusion([X1|L1],[X2|L2], [X2|R]):- X1>=X2,fusion([X1|L1],L2,R).
%17 diccionario(A,N) todas palabras A^n.
% Also:nmembers(A,N,L) dada L de N simbolos, escribe toda L pegada & provoca Backtrack
diccionario(A,N):- permut(A,N,L), writeWord(L).
diccionario(_,_).
permut(_,0,[]):-!.
permut(A,N,[X|L]):- pert(X,A), N1 is N-1, permut(A,N1,L).
writeWord([]):- write(' '), fail.
writeWord([X|L]):- write(X),writeWord(L).

%18. palíndromos(L) escribe palíndromos con elementos de L.
palíndromos(L):- permutacion(L, L1), esPalindromo(L1), write(L1), fail.
palíndromos(_).
ultimoElimina(X,L,R):- concat(R,[X],L).
esPalindromo([_]).
esPalindromo([]).
esPalindromo([X|L1]):- ultimoElimina(X,L1,MID), esPalindromo(MID).

% Con SETOF sería
% palíndromos(L) :- setof(P,(permutation(L,P), es_palindromo(P)),S), write(S).
% NOTA: sacado de soluciones ^^^^^

%19. SEND+MORE = MONEY, value of variables?
digSumConCarry(A,B,S,Cin,Carry):-
	S is (A+B+Cin) mod 10,
	Carry is (A+B+Cin) //10.
%send_more_money:-
%	pert_con_resto(S,[0,1,2,3,4,5,6,7,8,9], R0),
%	pert_con_resto(E, R0, R1),
%	pert_con_resto(N, R1, R2),
%	pert_con_resto(D, R2, R3),
%	pert_con_resto(M, R3, R4),
%	pert_con_resto(O, R4, R5),
%	pert_con_resto(R, R5, R6),
%	pert_con_resto(Y, R6, _),
%	digSumConCarry(D,E,Y,0, C0),
%	digSumConCarry(N,R,E,C0,C1),
%	digSumConCarry(E,O,N,C1,C2),
%	digSumConCarry(S,M,O,C2,M ),!,
%	write("  SEND:  "), 		write(S), write(E), write(N), write(D), nl,
%	write("+ MORE:  "), 		write(M), write(O), write(R), write(E), nl,
%	write(" MONEY: "),    write(M), write(O), write(N), write(E), write(Y), nl.
%send_more_money:- write("no").

% 19 bien (requería suma de listas de dígitos!)
sumListaDig([A|L1],[B|L2],[S|L3],Cin):-
	digSumConCarry(A,B,S,Cin,CarryNew),
	sumListaDig(L1,L2,L3,CarryNew).
sumListaDig([],[],[X],X). %NOTAR QUE SOLO SIRVE PARA SUMAS QUE DAN 1 DIG EXTRA.
send_more_money:-
	pert_con_resto(S,[0,1,2,3,4,5,6,7,8,9], R0),
	pert_con_resto(E, R0, R1),
	pert_con_resto(N, R1, R2),
	pert_con_resto(D, R2, R3),
	pert_con_resto(M, R3, R4),
	pert_con_resto(O, R4, R5),
	pert_con_resto(R, R5, R6),
	pert_con_resto(Y, R6, _),
	sumListaDig([D,N,E,S],[E,R,O,M],[Y,E,N,O,M],0),!,
	write("  SEND:  "), 		write(S), write(E), write(N), write(D), nl,
	write("+ MORE:  "), 		write(M), write(O), write(R), write(E), nl,
	write(" MONEY: "),    write(M), write(O), write(N), write(E), write(Y), nl.
send_more_money:- write("no").

% 20. Simplifica(EXP1, EXPSimple). (La idea es usar propiedades aritméticas! si puedo entrar, simplifica otra vez!)
% SOLO PARA * Y +, no más símbolos.
simplifica(E,E2):- simplificarUna(E,E3), !, simplifica(E3,E2).
simplifica(E, E).
% Todas deberían llevar ! porque si no, intentará otras cosas que llevan al mismo sitio.
simplificarUna(0*_, 0):-!.
simplificarUna(_*0, 0):-!.

simplificarUna(1*X, X):-!.
simplificarUna(X*1, X):-!.

simplificarUna(0+X, X):-!.
simplificarUna(X+0, X):-!.

simplificarUna(A+B, A+C):- simplificarUna(B,C), !.
simplificarUna(B+A, C+A):- simplificarUna(B,C), !.

simplificarUna(A*B, A*C):- simplificarUna(B,C), !.
simplificarUna(B*A, C*A):- simplificarUna(B,C), !.

simplificarUna(N1+N2, N3):- number(N1),number(N2), N3 is N1+N2, !.
simplificarUna(N1*N2, N3):- number(N1),number(N2), N3 is N1*N2, !.

%DISTRIBUTIVA (nota: sacado de soluciones que A y B deberían ser números, (tiene que ver con parentesis?) y necesito hacer todas las permutaciones (4)
simplificarUna(A*X+B*X, C*X):- number(A), number(B), C is A+B, !.
simplificarUna(A*X+X*B, C*X):- number(A), number(B), C is A+B, !.
simplificarUna(X*A+B*X, C*X):- number(A), number(B), C is A+B, !.
simplificarUna(X*A+X*B, C*X):- number(A), number(B), C is A+B, !.
%No incluímos asociativa porque no acabaría nunca, y por eso la mayoría de reglas salen dupli o cuadruplicadas.





