%%	%%%%%%
% restart prolog to clear out 1_2 before running this
%
beep_boop --> anything, beep(Suffix), anything, boop(Suffix), anything.

beep(X) -->
    "beep",
    suffix(X).

boop(X) -->
    "boop",
    suffix(X).

suffix([H|T]) -->
      [H],
      {
          code_type(H, digit)
      },
      suffix(T).
suffix([]) --> []. % must be 2nd suffix clause, or the digits wind up in anything
% At bottom for efficiency. At the top, would match beep first
anything --> [].
anything --> [_], anything.

% A subtlety here.  "foo 7 beep1 bar boop14 fdds" is part of the language
%
try_beep_boop :- phrase(beep_boop, X),format('~s~n', [X]).


%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  ; works for alternatives
article_phrase --> ("a" ; "an"),
	" ",
	noun.

noun --> "book".
noun --> "car".

%
% Look at all solutions
try_article_phrase :- phrase(article_phrase, X),format('~s~n', [X]).

