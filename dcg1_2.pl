% double quoted strings are just lists of UTF-8 codes,
% so they're terminals too
cliche -->
    thing,
    " is a ",
    type_of_thing,
    " trapped in a ",
    opposite_type_of_thing,
    " body.".
thing --> "Cygwin".
type_of_thing --> "Unix OS".
opposite_type_of_thing --> "Windows'".

% query try_literals. to see cliches
try_literals :- phrase(cliche, X),format('~s~n', [X]).

% you can unify arguments, just like normal prolog predicates
fizz_buzz(Msg) --> anything, fizz(Msg), anything, buzz, anything.
anything --> [].
anything --> [_], anything.
fizz(Msg) -->
    "fizz",
    {
        format('At fizz we have Msg=~w~n', [Msg])
    }.
buzz -->
    "buzz".

try_fizz_buzz :- phrase(fizz_buzz('howdy'), X),format('~s~n', [X]).

% The ; operator allows alternatives. This matches a book,
% an book, a car, or an car
%
article_phrase --> ("a" ; "an"),
	" ",
	noun.

noun --> "book".
noun --> "car".

try_article_phrase :-
	phrase(article_phrase, X),
	format('~s~n', [X]),
	fail.
try_article_phrase.

something(X) -->
      ({ is_wobbly(X) }  ->
               "a wobbly ",
               thing
        ;
               "a stable ",
               thing
       ).

is_wobbly(X) :-
	X = wobbly.

try_something :-
	phrase(something(wobbly), Generated),
	format('~s~n', [Generated]),
	fail.
try_something :-
	phrase(something(not_wobbly), Generated),
	format('~s~n', [Generated]),
	fail.
try_something.





