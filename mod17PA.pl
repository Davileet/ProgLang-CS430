%
% Hopscotch game: Imagine a version of hopscotch played on a linear course with markers in each square
% representing points. The goal is to hop through the course once, collecting a marker at each spot, to
% maximize the collected points. But there is a constraint: on each hop you must skip the next square or the
% next two squares. Hence from position i, the possible next position is either i+2 or i+3.
%
% Predicate hopscotch(X,R) is true if X is a list of integers representing score markers in the course and 
% R is a sequence of markers that leads to a maximum sequence of hops. Note that this predicate is expected
% to be used with a particular list for X and a variable or a list for R.
%
% This assignment is borrowed from Margaret Lamb, Queens University Computer Science Department, Ontario.
%
% David Thompson

% hopscotch(X,R) is true if X is a list of numbers and R is the list of values from X that win the hopscotch game.   
sum([],0).
sum([H|T],S) :- sum(T,E), S is H+E. 

hopscotch([],[]).			%Empty set base case.
hopscotch([X],[X]).			%Single num list equality base case.
hopscotch([Y,_],[Y]).		%Two list nodes base case.
hopscotch([Y,_,Z],[Y,Z]).	%Three list nodes base case.

hopscotch([A,_,C,D|Tail], Result) :- hopscotch([C, D|Tail], Path1), hopscotch([D|Tail], Path2), sum(Path1, S1), sum(Path2, S2), S1 > S2,  append([A], Path1, Result). %If Path1 is larger than path2.

hopscotch([A,_,C,D|Tail], Result) :- hopscotch([C, D|Tail], Path1), hopscotch([D|Tail], Path2), sum(Path1, S1), sum(Path2, S2), S1 =< S2, append([A], Path2, Result). %If Path2 is larger than path1.

%%%%%%%%%%%%%%%%% Tests

test_hopscotch :-
  hopscotch([],[]),
  hopscotch([1],[1]),
  hopscotch([1,2],[1]),
  hopscotch([1,2,3],[1,3]),
  hopscotch([1,2,4,1],[1,4]),
  hopscotch([1,3,2,6,5],[1,2,5]),
  hopscotch([7,4,5,9,6,1,2,3],[7,5,6,3]),
  hopscotch([6,2,4,8,3,2,3,4,3,5,2,5,3,5,9,1],[6,8,2,4,5,5,9]),
  hopscotch([6,2,4,8,3,2,3,4,3,5,2,5,3,5,9,5],[6,8,2,4,5,5,5,5]).

test :- test_hopscotch, !.


