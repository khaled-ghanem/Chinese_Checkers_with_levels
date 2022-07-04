



%Helper Function
member(X,[X|_]):-!.
member(X,[_|T]):-
	member(X,T).

append([],L2,L2).
append([H|T],L2,[H|R]):-
	append(T,L2,R).



abs2(X,Y) :- X < 0 -> Y is -X ; Y = X.

removeFromList(_, [], []):-!.
removeFromList(H, [H|T], V):-!,
	 removeFromList(H, T, V).
removeFromList(H, [H1|T], [H1|T1]):-
	removeFromList(H, T, T1).

/*
%%%%%%%%%%%%%%%%%%%%%%%% Problem 1 Using DFS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%Representation goal:-[One , Two , Three] "sum of them == total"




rest(E, [H|T], Remain) :-
    restOfList(T, H, E, Remain).

restOfList(T, H, H, T).
restOfList([H|T], _, E, Result) :-
    restOfList(T, H, E, Result).

%moves
move(State, [One,Two, Three]):- 
    	rest(One, State, Lst1),
	rest(Two, Lst1, Lst2),
      rest(Three, Lst2, _).


%Rules : NO Rules
isOkay(_).

getNextState(State,Open,Closed,NextState):-
	move(State,NextState),
	not(member(NextState,Open)),
	not(member(NextState,Closed)),
	isOkay(NextState).
getAllValidChildren(State,Open,Closed,Children):-
	findall(X,getNextState(State,Open,Closed,X),Children).


search(Open,_,Goal, [A, B, C]):-
	Open=[[A,B,C]|_],
      Goal is A + B + C .

search(Open,Closed,Goal,Output):-
	Open=[State|RestOfOpen],
	getAllValidChildren(State,Open,Closed,Children),
	append(Children,RestOfOpen,NOpen),%DFS 
	append([State],Closed,NClosed),
	search(NOpen,NClosed,Goal,Output).




threeSum(List, Goal, Output):-

    search([List], [], Goal, Output).


%%%%%%%%%%%%%%%%%%%%%%%% Problem 1 Using GREADY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%Representation goal:-[One , Two , Three] "sum of them == total"




rest(E, [H|T], Remain) :-
    restOfList(T, H, E, Remain).

restOfList(T, H, H, T).
restOfList([H|T], _, E, Result) :-
    restOfList(T, H, E, Result).

%moves
move(State, [One,Two, Three]):- 
    	rest(One, State, Lst1),
	rest(Two, Lst1, Lst2),
      rest(Three, Lst2, _).
 

%Rules : NO Rules
isOkay(_).

%Heuristic
getHeuristic(State  , H,Goal):-
State = [One ,Two, Three],
Total is One + Two + Three,
He is Goal-Total ,
abs2(He,H).


getBestChild([Child], Child, []).
getBestChild(Open, Best, RestOpen):-
	getBestChild1(Open, Best),
	removeFromList(Best, Open, RestOpen).

getBestChild1([State], State):-!.
getBestChild1([State|Rest], Best):-
	getBestChild1(Rest, Temp),
	getBest(State, Temp, Best).

getBest([State,  H], [_,  H1], [State,  H]):-
	H < H1, !.
getBest([_,  _], [State1, HN], [State1,  HN]).


getNextState(State,Open,Closed,NextState):-
	move(State,NextState),
	not(member(NextState,Open)),
	not(member(NextState,Closed)),
	isOkay(NextState).
getAllValidChildren(State,Open,Closed,Children):-
	findall(X,getNextState(State,Open,Closed,X),Children).


search(Open,_,Goal, [A, B, C]):-
	Open=[[A,B,C]|_],
      Goal is A + B + C .

search(Open,Closed,Goal,Output):-
	getBestChild(Open, [State , H], RestOpen),
	getAllValidChildren(State,Open,Closed,Children),
	append(Children,RestOpen,NOpen),
	append([State],Closed,NClosed),
	getHeuristic(State, H, Goal),
	search(NOpen,NClosed,Goal,Output).




threeSum(List, Goal, Output):-
	move(List, [One,Two, Three]), 
	getHeuristic([One,Two, Three], H, Goal),
    	search([[One,Two, Three],H], [], Goal, Output).



*/

%%%%%%%%%%%%%%%%%%%%%%%% Problem 2 Using GREADY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%Representation goal:-[ List of Characters ] 

delete(X,[X|T],T):-!.
delete(X,[Y|T],[Y|T1]):-delete(X,T,T1).

%Rules : NO Rules
isOkay(_).


%moves

move([State,_] ,NState):-
	delete('A',State,NState);
	delete('B',State,NState);
	delete('C',State,NState);
	delete('D',State,NState);
	delete('E',State,NState);
	delete('F',State,NState);
	delete('G',State,NState);
	delete('H',State,NState);
	delete('I',State,NState);
	delete('J',State,NState);
	delete('K',State,NState);
	delete('L',State,NState);
	delete('M',State,NState);
	delete('N',State,NState);
	delete('O',State,NState);
	delete('P',State,NState);
	delete('Q',State,NState);
	delete('R',State,NState);
	delete('S',State,NState);
	delete('T',State,NState);
	delete('U',State,NState);
	delete('V',State,NState);
	delete('W',State,NState);
	delete('X',State,NState);
	delete('Y',State,NState);
	delete('Z',State,NState).





%Heuristic
getHeuristic(_, 0, []):-!.
getHeuristic([],0,[]):-!.
getHeuristic([],0,_):-!.
getHeuristic([H|T1],V,[H|T2]):-!,
	getHeuristic(T1,V, T2).

getHeuristic(T1,H,[_|T2]):-
	getHeuristic(T1,TH, T2),
	H is TH + 1.



getNextState(State,Open,Closed,Goal,[NextState,H]):-
	move(State,NextState),
	getHeuristic(NextState, H, Goal),	
	not(member([NextState,H],Open)),
	not(member([NextState,H],Closed)),
	isOkay([NextState,H]).
	
getAllValidChildren(State,Open,Closed,Goal,Children):-
	findall(X,getNextState(State,Open,Closed,Goal,X),Children).

		
getBestChild([Child], Child, []).
getBestChild(Open, Best, RestOpen):-
	getBestChild1(Open, Best),
	removeFromList(Best, Open, RestOpen).

getBestChild1([State], State):-!.
getBestChild1([State|Rest], Best):-
	getBestChild1(Rest, Temp),
	getBest(State, Temp, Best).

getBest([State,H], [_,  H1], [State, H]):-
	H< H1, !.
getBest([_,_], [State1,  NH], [State1,NH]).





search(Open,_,Goal):-
	Open=[[State,_]|_],
	State = Goal,!.
	

search(Open,Closed,Goal):-
	getBestChild(Open, Best , RestOpen),
	getAllValidChildren(Best,Open,Closed,Goal,Children),
	append(Children,RestOpen,NOpen),
	append(Best,Closed,NClosed),
	search(NOpen,NClosed,Goal).



deletiveEditing(State,Goal):-
	getHeuristic(State, H, Goal),
	search([[State,H]],[],Goal).


