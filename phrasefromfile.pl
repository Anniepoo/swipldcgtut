
% needed for phrase_from_file
:- use_module(library(pure_input)).

... --> []|[_],... .

file_contains(File, Pattern) :-
        phrase_from_file((..., Pattern, ...), File).

match_count(File, Pattern, Count) :-
        findall(x, file_contains(File, Pattern), Xs),
        length(Xs, Count).

