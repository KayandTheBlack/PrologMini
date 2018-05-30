padre(juan,pedro).
padre(maria,pedro).
hermano(pedro,vicente).
hermano(pedro,alberto).
tio(X,Y):-padre(X,Z),hermano(Z,Y).

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
