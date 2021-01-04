isMexican(carnitas).
isDelicious(X) :- isMexican(X).

hasType(t,bool).
hasType(f,bool).
hasType(num, int).
hasType(add(X,Y),int) :- hasType(X,int), hasType(Y, int).


%%%%%%%%%%%%
%% Family %%
%%%%%%%%%%%%

parent(kim, holly).  
parent(margaret, kim).  
parent(herbert, margaret).
parent(john, kim).
parent(felix, john).  
parent(albert, felix).
parent(albert, dana).
parent(felix, maya).

grandparent(GP, GC) :- parent(GP, P), parent(P, GC).
greatgrandparent(GGP, GC) :- parent(GGP, GP), grandparent(GP, GC).

has_family(X) :- parent(X, _).
has_family(X) :- parent(_, X).

ancestor(A,X) :- parent(P,X),ancestor(A,P).
ancestor(A,X) :- parent(A,X).

sibling(X, Y) :- parent(P,X), parent(P,Y), not(X = Y).

%%%%%%%%%%%%%%%%
%% Arithmetic %%
%%%%%%%%%%%%%%%%
% A
% addP(X, Y) :- _ is X + Y.

% B
addP(X, Y, Z) :- Z is X + Y.

% C
% addP(X, Y, X + Y).

% D
% addP(X, Y) :- X + Y.

% E
% addP(X, Y, Z) :- X + Y is Z.

fib(0,1).
fib(1,1).
fib(N,R) :- N > 1
          , N1 is N - 1
          , N2 is N - 2 
          , fib(N1,R1)
          , fib(N2,R2)
          , R is R1 + R2.


%%%%%%%%%%%
%% Lists %%
%%%%%%%%%%%


headOf([H|_],H).
tailOf([_|T],T).


isIn(X,[X|_]).
isIn(X,[_|T]) :- isIn(X,T).

sum([],0).
sum([H|T],R) :- sum(T,R1), R is H + R1.

append([],Ys,Ys).
append([H|T],Ys,[H|Zs]) :- append(T,Ys,Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Farmer, Wolf, Goat, Cabbage %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change(east,west).
change(west,east).

move([X,X,P_Goat,P_Cab],     move_wolf,   [Y,Y,P_Goat,P_Cab])      :- change(X,Y).
move([X,P_Wolf,X,P_Cab],     move_goat,   [Y,P_Wolf,Y,P_Cab])      :- change(X,Y).
move([X,P_Wolf,P_Goat,X],    move_cabbage,[Y,P_Wolf,P_Goat,Y])     :- change(X,Y).
move([X,P_Wolf,P_Goat,P_Cab],move_nothing,[Y,P_Wolf,P_Goat,P_Cab]) :- change(X,Y).

safe([P_Farmer,P_Wolf,P_Goat,P_Cab]) :-
  one_equal(P_Farmer,P_Wolf,P_Goat),
  one_equal(P_Farmer,P_Goat,P_Cab).
  
one_equal(X,X,_).
one_equal(X,_,X).

solution([east,east,east,east],[]).
solution(State,[FirstMove|RemainingMoves]) :-
  move(State,FirstMove,NextState),
  safe(NextState),
  solution(NextState,RemainingMoves).

