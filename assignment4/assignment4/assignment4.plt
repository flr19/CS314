:- include(assignment4).

:- begin_tests(range).

test('range1', [true]) :- range(1,2,2).
test('range2', [true]) :- not(range(1,2,3)).

:- end_tests(range).



:- begin_tests(reverseL).

test('reverse1', [true(X == [ ])]) :- reverseL([],X).
test('reverse2', [true(X == [3, 2, 1])]) :- reverseL([1,2,3],X).
test('reverse3', [true(X == [c, b, a])]) :- reverseL([a,b,c],X).

:- end_tests(reverseL).



:- begin_tests(memberL).

test('memberL1', [true]) :- not(memberL(1, [])).
test('memberL2', [true]) :- memberL(1, [1,2,3]).
test('memberL3', [true]) :- not(memberL(4,[1,2,3])).
test('memberL4', all(X == [1,2,3])) :- memberL(X, [1,2,3]).

:- end_tests(memberL).



:- begin_tests(zip).

test('zip1', [true(Z == [1-a, 2-b])]) :- zip([1,2],[a,b],Z).
test('zip2', [true(Z == [a-1, b-X, c-y])]) :- zip([a,b,c,d],[1,X,y], Z).
test('zip3', [true(Z == [a-1, b-X, c-y])]) :- zip([a,b,c],[1,X,y,z], Z).
test('zip4', [true(A-B == [1,2]-[a,b])]) :- length(A,2), length(B,2), zip(A, B, [1-a, 2-b]).

:- end_tests(zip).



:- begin_tests(insert).

test('insert1', [true(L==[2,3,4,5])]) :- insert(3, [2,4,5], L).
test('insert2', [true(L==[1,2,3,3])]) :- insert(3, [1,2,3], L).
test('insert3', [fail]) :- insert(3, [1,2,4], [1,2,3]).
test('insert4', [true(L == [2,4,5])]) :- insert(3, L, [2,3,4,5]).
test('insert5', [true(L == [1,3,6])]) :- insert(9, L, [1,3,6,9]).
test('insert6', [true(L == [1,3,5])]) :- insert(3, L, [1,3,3,5]).

:- end_tests(insert).



:- begin_tests(remove_duplicates).

test('remove_duplicates1', [true(X == [1,2,3,4])]) :- remove_duplicates([1,2,3,4,2,3],X).
test('remove_duplicates2', [true(X == [1, 4, 5, 2, 7, 3])]) :- remove_duplicates([1,4,5,4,2,7,5,1,3],X).
test('remove_duplicates3', [true(X == [ ])]) :- remove_duplicates([ ], X).

:- end_tests(remove_duplicates).



:- begin_tests(intersectionL).

test('intersectionL1', [true]) :- intersectionL([1,2,3,4],[1,3,5,6],[1,3]).
test('intersectionL2', [true(X == [1,3])]) :- intersectionL([1,2,3,4],[1,3,5,6],X).
test('intersectionL3', [true]) :- intersectionL([1,2,3],[4,3],[3]).

:- end_tests(intersectionL).



:- begin_tests(partition).

test('partition1', [true]) :- partition([a],[a],[]).
test('partition2', [true]) :- partition([1,2,3],[1],[2,3]).
test('partition3', [true(X-Y == [a,b]-[c,d])]) :- partition([a,b,c,d],X,Y).

:- end_tests(partition).



:- begin_tests(merge).

test('merge1', [true]) :- merge([],[1],[1]).
test('merge2', [true]) :- merge([1],[],[1]).
test('merge3', [true(X == [1,2,3,4,5,6])]) :- merge([1,3,5],[2,4,6],X).

:- end_tests(merge).



:- begin_tests(mergesort).

test('mergesort1', [true(X == [1,2,3])]) :- mergesort([3,2,1],X).
test('mergesort2', [true(Y == [1,2,3])]) :- mergesort([1,2,3],Y).
test('mergesort3', [true(Z == [ ])]) :- mergesort([],Z).
test('mergesort4', [true(X == [1,2,3,4,5,6])]) :- mergesort([1,3,5,2,4,6],X).

:- end_tests(mergesort).
