% written by Sanal Alick Lisboa
%z5195127 UNSW
:- style_check(-singleton).
adjacent((X,Y),(X1,Y1)):-
    Y1 is Y+1,
	X1 is X.
adjacent((X,Y),(X1,Y1)):-
    Y1 is Y-1,
	X1 is X.
adjacent((X,Y),(X1,Y1)):-
    X1 is X-1,
	Y1 is Y.
adjacent((X,Y),(X1,Y1)):-
    X1 is X+1,
	Y1 is Y.


%land_or_dropped(X,Y) :- land(X,Y).

%land_or_dropped(X,Y) :- dropped(X,Y).

s((X,Y),(XX,YY),1):-
   adjacent((X,Y),(XX,YY)),
   land_or_dropped(XX,YY).
   

s((X,Y),(XX,YY),1000):-
   adjacent((X,Y),(XX,YY)),
   not(land_or_dropped(XX,YY)).


% pathsearch.pl : We have used the path search code as a helper function
% for UCSD algorithm.

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair
% This file provides code for insert_legs(), head_member() and build_path().

% insert_legs(Generated, Legs, Generated1).
% insert new legs into list of generated legs,
% by repeatedly calling insert_one_leg()

% base case: no legs to be inserted
insert_legs(Generated, [], Generated).

% Insert the first leg using insert_one_leg(); and continue.
insert_legs(Generated, [Leg|Legs], Generated2) :-
   insert_one_leg(Generated, Leg, Generated1),
   insert_legs(Generated1, Legs, Generated2).

% head_member(Node, List)
% check whether Node is the head of a member of List.

% base case: node is the head of first item in list.
head_member(Node,[[Node,_]|_]).

% otherwise, keep searching for node in the tail.
head_member(Node,[_|Tail]) :-
  head_member(Node,Tail).

% build_path(Expanded, [[Node,Pred]], Path).

% build_path(Legs, Path)
% Construct a path from a list of legs, by joining the ones that match.

% base case: join the last two legs to form a path of one step.
build_path([[Next,Start],[Start,Start]], [Next,Start]).

% If the first two legs match, add to the front of the path.
build_path([[C,B],[B,A]|Expanded],[C,B,A|Path]) :-
   build_path([[B,A]|Expanded],[B,A|Path]), ! .

% If the above rule fails, we skip the next leg in the list.
build_path([Leg,_SkipLeg|Expanded],Path) :-
   build_path([Leg|Expanded],Path).


% Now we have UCSD algorithm which with some modifications can be used
% in our question to arrive at an optimal path.

%Uniform Cost Search, using Dijkstras Algorithm
% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% solve(Start, Solution, G, N)
% Solution is a path (in reverse order) from start node to a goal state.
% G is the length of the path, N is the number of nodes expanded.

% Now here I have made some modifications so that we can allow this
% algorithm to work with locations in the form (X,Y).
% modifications done: start ---> (StartX,StartY)
%                     Node ---> (NodeX,NodeY).

solve((X,Y), Solution, G, N)  :-!,
    ucsdijkstra([[(X,Y),(X,Y),0]], [], Solution, G, 1, N),!.


% ucsdijkstra(Generated, Expanded, Solution, L, N)
%
% The algorithm builds a list of generated "legs" in the form
% Generated = [[Node1,Prev1,G1],[Node2,Prev2,G2],...,[Start,Start,0]]
% The path length G from the start node is stored with each leg,
% and the legs are listed in increasing order of G.
% The expanded nodes are moved to another list (G is discarded)
%  Expanded = [[Node1,Prev1],[Node2,Prev2],...,[Start,Start]]

% If the next leg to be expanded reaches a goal node,
% stop searching, build the path and return it.
ucsdijkstra([[(X,Y),Pred,G]|_Generated], Expanded, Path, G, N, N)  :-
    agent_at(X,Y),
    build_path([[(X,Y),Pred]|Expanded], Path).

% Extend the leg at the head of the queue by generating the
% successors of its destination node.
% Insert these newly created legs into the list of generated nodes,
% keeping it sorted in increasing order of G; and continue searching.
ucsdijkstra([[Node,Pred,G]| Generated], Expanded, Solution, G1, L, N) :-
    extend(Node, G, Expanded, NewLegs),
    M is L + 1,
    insert_legs(Generated, NewLegs, Generated1),
    ucsdijkstra(Generated1, [[Node,Pred]|Expanded], Solution, G1, M, N).

% Find all successor nodes to this node, and check in each case
% that the new node has not previously been expanded.
extend(Node, G, Expanded, NewLegs) :-
    findall([NewNode, Node, G1], (s(Node, NewNode, C)
    , not(head_member(NewNode, Expanded))
    , G1 is G + C
    ), NewLegs).

% base case: insert leg into an empty list.
insert_one_leg([], Leg, [Leg]).


% If we already knew a shorter path to the same node, discard the new one.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated]) :-
    Leg  = [Node,_Pred, G ],
    Leg1 = [Node,_Pred1,G1],
    G >= G1, ! .

% Insert the new leg in its correct place in the list (ordered by G).
insert_one_leg([Leg1|Generated], Leg, [Leg,Leg1|Generated]) :-
    Leg  = [_Node, _Pred, G ],
    Leg1 = [_Node1,_Pred1,G1],
    G < G1, ! .

% Search recursively for the correct place to insert.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated1]) :-
    insert_one_leg(Generated, Leg, Generated1).


initial_intentions(intents(Intentions,[])):-!,
	monster(X,Y),
	I is 0,
	solve((X,Y),O,G,N),
	initial(O,[],Intentions),!.

	

initial([(X,Y)|Tail], R, Intentions):-!,
    not(land(X,Y)) ->
    append(R,[[goal(X,Y),[]]],S),
	initial(Tail, S, Intentions),!;
	initial(Tail, R, Intentions),!.
initial([],R,Intentions):-!,
    append(R,[],Intentions).

    
    
    
trigger([],[]):-!.
trigger([stone(X,Y)],L):-
   append([goal(X,Y)],[],S),
   trigger([],S,L),!.
    
trigger([stone(X, Y)|Events],L) :-!,
    append([goal(X,Y)],[],R),
    trigger(Events,R,L),!.
trigger([stone(X, Y)|Events],R,L) :-!,
    append([goal(X,Y)],R,S),
	trigger(Events,S,L),!.
trigger([],R,L) :-!,
    append([],R,S),
	reverse(S,L),!.

	
incorporate_goals([], Intentions,Intentions):-!.

incorporate_goals(Goal, Intentions, Intentions1):-!,
    incorporate_goals1(Goal, Intentions, I, Intentions1),!.

incorporate_goals1([], intents(L,Plan), S, intents(L,Intentions1)).

incorporate_goals1([Goal|Tail], intents(L,Plan), S, intents(L,Intentions1)) :-
    (is_member(Goal,L);is_member(Goal,Plan)) ->
	Intentions1 = Plan;
    insert_goal(Goal, I ,Tail , L, Intentions1, S),
	length(Tail,Len),
	append(I,S,P),
	(Len > 0 
	-> incorporate_goals1(Tail, intents(L,Plan), P, intents(L,Intentions1)),!
	;append(P,Plan,X),
	mergeSort(X,Q),
	append(Q,[],Intentions1)).

is_member(Goal, [[Head,P]|_]) :-!,
    member(Goal, [Head]).

is_member(Goal, [[Head,P]|Tail]) :-!,
    member(Goal, [Head]),
	is_member(Goal, Tail),!.
	
	
insert_goal(Goal, I, Tail, L, Intentions1, S):-
    check(Goal)-> 
    insert_goal1(Goal, I);
	Z is 2.

insert_goal1(X, [[X, []]]).
	
halve([], [], []).
halve([A], [A], []).
halve([A,B|Cs], [A|X], [B|Y]) :- halve(Cs, X, Y).

merge([], Ys, Ys).
merge(Xs, [], Xs).
merge([[goal(X1,X2),P]|Xs], [[goal(Y1,Y2),Q]|Ys], M) :-
   (  
      agent_at(X,Y),
	  distance((X,Y),(X1,X2),D),
	  distance((X,Y),(Y1,Y2),E),
      D =< E -> M = [[goal(X1,X2),P]|Ms], merge(Xs, [[goal(Y1,Y2),Q]|Ys], Ms) 
    ; M = [[goal(Y1,Y2),[]]|Ms], merge([[goal(X1,X2),Q]|Xs], Ys, Ms)
   ).

mergeSort([], []).
mergeSort([E], [E]).
mergeSort([E1,E2|Es], SL) :- 
     halve([E1,E2|Es], L1, L2),
     mergeSort(L1, SL1),
     mergeSort(L2, SL2),
     merge(SL1, SL2, SL).     


check(goal(X1, Y1)) :-
    not(agent_at(X1,Y1)),
    solve((X1,Y1),H,G,N),!,
	G < 1000.


reverse(Xs,Ys) :- reverse(Xs,[],Ys).

reverse([],A,A).
reverse([H|T],R,A) :- reverse(T,[H|R],A).	

get_action(intents([[goal(X,Y),[]]|Inttail],[]), Intentions1, Action):-!,
    agent_stones(0)->
	Intentions1 = intents([[goal(X,Y),[]]|Inttail],[]),
	agent_at(XA,YA),
	Action = move(XA,YA);
	solve((X,Y),[Head|Tail],_,_),!,
	get_path(Tail,S,Action,S),!,
	Intentions1 = intents([[goal(X,Y),S]|Inttail],[]).
	
get_action(intents([[goal(X,Y),P]|Inttail],[]), Intentions1, Action):-
    length(P,Len),
	((agent_stones(1), Len < 1) ->
	solve((X,Y),[Head|Tail],_,_),!,
	get_path(Tail,S,Action,S),!,
	Intentions1 = intents([[goal(X,Y),S]|Inttail],[])
	;((agent_stones(1), Len > 0) ->
	get_action1(intents([[goal(X,Y),P]|Inttail],[]), Intentions1, Action))).
	
get_action(intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),Path]|PlanTail]), Intentions1, Action):-
    length(P,Len),
	length(Path,L2),
	((agent_stones(1), Len < 1) ->
	solve((X,Y),[Head|Tail],_,_),!,
	get_path(Tail,S,Action,S),!,
	Intentions1 = intents([[goal(X,Y),S]|Inttail],[[goal(X1,Y1),Path]|PlanTail])
	;((agent_stones(1), Len > 0) ->
	get_action1(intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),Path]|PlanTail]), Intentions1, Action))
	;((agent_stones(0), L2 < 1) ->
	get_action2(intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),Path]|PlanTail]), Intentions1, Action)),!
	;((agent_stones(0), L2 > 0) ->
	get_action3(intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),Path]|PlanTail]), Intentions1, Action),!)).
	
get_action(intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),Path]]), Intentions1, Action):-
    length(P,Len),
	length(Path,L2),
	((agent_stones(1), Len < 1) ->
	solve((X,Y),[Head|Tail],_,_),!,
	get_path(Tail,S,Action,S),!,
	Intentions1 = intents([[goal(X,Y),S]|Inttail],[[goal(X1,Y1),Path]])
	;((agent_stones(1), Len > 0) ->
	get_action1(intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),Path]]), Intentions1, Action))
	;((agent_stones(0), L2 < 1) ->
	get_action21(intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),Path]]), Intentions1, Action)),!
	;((agent_stones(0), L2 > 0) ->
	get_action31(intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),Path]]), Intentions1, Action),!)).
	
get_action3(intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),[Headp|Tailp]]|PlanTail]), Intentions1, Action):-
	Action = Headp,
	Intentions1 = intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),Tailp]|PlanTail]).

get_action31(intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),[Headp|Tailp]]]), Intentions1, Action):-
	Action = Headp,
	Intentions1 = intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),Tailp]]).
	
get_action2(intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),Path]|Tail]), Intentions1, Action):-
    solve((X1,Y1),[Head|T],_,_),!,
	get_path1(T,S,Action,S),!,
	Intentions1 = intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),S]|Tail]).

get_action21(intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),Path]]), Intentions1, Action):-
    solve((X1,Y1),[Head|T],_,_),!,
	get_path1(T,S,Action,S),!,
	Intentions1 = intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),S]]).
	
get_action1(intents([[goal(X,Y),[Headp|Tailp]]|Inttail],Plan), Intentions1, Action):-
	Action = Headp,
	Intentions1 = intents([[goal(X,Y),Tailp]|Inttail],Plan).

get_path([(X, Y)|Events],L, Action, S) :-
    Action = move(X,Y),
    append([],[],R),
    get_path(Events,R,L),!.
get_path([(X, Y)|Events],R,L) :-
    append([move(X,Y)],R,S),
	get_path(Events,S,L),!.
get_path([(X, Y)],R,L) :-
    append([drop(X,Y)],R,S),
	reverse(S,L),!.

get_path1([(X, Y)|Events],L, Action, S) :-
    Action = move(X,Y),
    append([],[],R),
    get_path1(Events,R,L),!.
get_path1([(X, Y)|Events],R,L) :-
    append([move(X,Y)],R,S),
	get_path1(Events,S,L),!.
get_path1([(X, Y)],R,L) :-
    append([pick(X,Y)],R,S),
	reverse(S,L),!.
	
update_intentions(at(A,B), Intentions, Intentions).
    
update_intentions(picked(A,B), intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),S]|Tail]), Intentions1):-
	Intentions1 = intents([[goal(X,Y),P]|Inttail],Tail).
update_intentions(dropped(A,B), intents([[goal(X,Y),P]|Inttail],[[goal(X1,Y1),S]|Tail]), Intentions1):-	
	Intentions1 = intents(Inttail,[[goal(X1,Y1),S]|Tail]).
update_intentions(dropped(A,B), intents([[goal(X,Y),P]|Inttail],[]), Intentions1):-	
	Intentions1 = intents(Inttail,[]).
