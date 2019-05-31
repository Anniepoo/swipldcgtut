% DCG that recognizes a list of a's
as --> [].
as --> [a], as.

%
% Run to produce a list according to the grammar "as", into the variable Ls, by querying
% ?- phrase(as, Ls).


%
% Exercise 1:
% Add another DCG that creates an alternating series of a's and b's:

as --> [].
as --> [a], bs.

bs --> [].
bs --> [b], as.

%
% Run to produce a list
% ?- phrase(as, Ls).


%
% Exercise 2
%
% test the list [a,a,a] to see that it does match the grammar "as", by querying
% ?- phrase(as, [a,a,a]).
% true.

%
% test the list [b,c,d] to see that it does not match the grammar "as", by querying
% ?- phrase(as, [b,c,d]).
% false.

%
% ask Prolog to solve X so that the list [a,X,a] will match the grammar "as", by querying
% ?- phrase(as, [a,X,a]).
% X = a.

