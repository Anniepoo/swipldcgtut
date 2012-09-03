
% needed for phrase_from_file
:- use_module(library(pure_input)).


file_contains(File, Pattern) :-
        phrase_from_file((..., Pattern, ...), File).

match_count(File, Pattern, Count) :-
        findall(x, file_contains(File, Pattern), Xs),
        length(Xs, Count).



find_zombie(File) :-
	phrase_from_file(anything_but_zombie, File).

anything_but_zombie -->



... --> []|[_],... .
