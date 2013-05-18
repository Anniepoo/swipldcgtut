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

