
% needed for phrase_from_file
:- use_module(library(pure_input)).

% matches anything.  | is an alternative to ;
... --> []|[_],... .

% general pattern to match a subportion of a file
file_contains(File, Pattern) :-
        phrase_from_file((... , Pattern, ...), File).

% count the matches
match_count(File, Pattern, Count) :-
        findall(x, file_contains(File, Pattern), Xs),
        length(Xs, Count).

% query  match_count('testfile.txt', "the", Count).
%
