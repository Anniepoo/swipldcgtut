:- use_module(pure_input).
:- use_module(library(readutil)).

:- multifile prolog:message/3.

prolog:message(error(parse_error(ErrText, Token), _Context)) -->
	['~w: ''~w'''-[ErrText, Token] ].

parse_error(Error, Token) -->
	{ throw(error(parse_error(Error, Token), _Context)) }.

nothing("") --> "".
nl --> "\n".

string(A) --> nl, nothing(A).
string(0'?) --> "zombie", parse_error('illegal reserved word', zombie).
string([A | B]) -->
	[A], string(B).

string_chars(A) --> string(B), {atom_codes(A, B) }.


:- meta_predicate safely(0).

safely(Goal) :-
	catch(Goal, Err, (print_message(error, Err), fail)).

test_pio :-
	read_file_to_codes('xterm_zombie_bug.pl', Codes, [type(text)]),
	safely(phrase(string_chars(A), Codes)),
	writeln(parsed(A)).


test_pio1 :-
	safely(phrase_from_file(string_chars(A), 'xterm_zombie_bug.pl', [type(text)])),
	writeln(parsed(A)).

test_pio1a :-
	safely(phrase_from_file(string_chars(A), 'xterm_zombie_bug.pl', [type(binary)])),
	writeln(parsed(A)).

