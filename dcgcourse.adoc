# Using Defnite Clause Grammars in SWI-Prolog
by [Markus Triska](mailto:markus.triska@gmx.at)
and [Anne Ogborn](mailto:aogborn@uh.edu)

<a href='#anchintro'>Introduction</a>
<a href='#anch1'>1 Definite Clause Grammars</a>
<a href='#anch2'>2 Relating Trees To Lists</a>
<a href='#anch3'>3 Left Recursion</a>
<a href='#anch4'>4 Right-hand Context Notation</a>
<a href='#anch5'>5 Implicitly Passing States Around</a>
<a href='#anch6'>6 Parsing From Files</a>
<a href='#anch7'>7 Implementation</a>
<a href='#anch8'>8 Using DCGs</a>
<a href='#anchanchconclusion'>Conclusion</a>

## Introduction
<a id='anchintro' />
### Who This Course Is For

This course is for anyone who knows swi-Prolog reasonably well and wants to effectively generate or parse lists. Notice that those two items go far beyond the canonical DCG task of parsing text.

### Getting The Most From This Course

This course is this web page and a series of example programs.

The example programs are *not* reproduced here. I want you to actually _look at_ and _fiddle with_ the code. So hopefully you'll be encouraged if you have to read it locally. You can get the examples [from github](https://github.com/Anniepoo/swipldcgtut)

To get the most from this course, you'll need to

 * Have a working [swi-Prolog](http://www.swi-prolog.org) install 
 * Get the example files from [from github](https://github.com/Anniepoo/swipldcgtut)
 * Understand basic Prolog be able to use SWI-Prolog's environment
 * Read the text
 * Try each example program.  Experiment!
 * Do the exercises

The example programs are labelled with the chapter and section number, so dcg1_2.pl is the code for chapter one section 2.

Different people will have different backgrounds and learning styles. Whatever works for you works.

### Getting Stuck

If you have questions and reasonable effort doesn't answer them, drop me email at aogborn (somechar) uh.edu. Please, if you're taking a beginning Prolog course, ask your instructor. Questions about family trees will be ignored. But if you're working on a real DCG related problem, feel free.

Asking on ##Prolog on freenode.net IRC is also a good way to get answers.

## 1 Definite Clause Grammars
<a id='anch1' />

A Prolog _definite clause grammar_ (DCG) describes a Prolog list. Operationally, DCGs can be used to parse and generate lists. A DCG is defined by _DCG rules_. A DCG rule has the form: 

    Head --> Body.

A rule's head is a Prolog term that is not a variable. A DCG head with functor f and arity N is referred to as <code>f//N</code> to distinguish it from a regular Prolog predicate, which we refer to as <code>f/N</code>. A rule's body is a sequence of terminals and nonterminals, separated by (,)/2 (comma operator). A terminal is a Prolog list, which stands for the elements it contains. A nonterminal refers to a DCG rule or other language construct, which stand for the elements they themselves describe. Declaratively, we can read the comma as _"and then"_ in DCGs. 


As an example, let us describe lists that only contain the atom 'a'. We shall use the nonterminal <code>as//0</code> to refer to such lists: 

       as --> [].
       as --> [a], as.
       
The first rule says: The empty list is such a list. The second rule says: A list containing the atom 'a' _and then_ only atoms 'a' is also such a list.

To execute a grammar rule, we use Prolog's `built-in phrase/2` predicate. The first argument is a DCG body. `phrase(Body, Ls)` is true iff Body describes the list Ls.

The most general query asks for all solutions:

      ?- phrase(as, Ls).
      Ls = [] ;
      Ls = [a] ;
      Ls = [a, a] ;
      Ls = [a, a, a] ;
      Ls = [a, a, a, a] ;
      etc.
    

Examples of more specific queries and the system's answers:

      ?- phrase(as, [a,a,a]).
      true.

      ?- phrase(as, [b,c,d]).
      false.

      ?- phrase(as, [a,X,a]).
      X = a.

## 2 Relating Trees To Lists
<a id='anch2' />

Let us now use a DCG to relate a binary tree to the in-order sequence of its node names. Let us assume a binary tree consists of leaves of the form `nil` and inner nodes of the form node(Name, Left, Right), where Left and Right are themselves binary trees. To obtain the in-order sequence of node names, consider:

      tree_nodes(nil) --> [].
      tree_nodes(node(Name, Left, Right)) -->
              tree_nodes(Left),
              [Name],
              tree_nodes(Right).
    

Example:

      ?- phrase(tree_nodes(node(a, node(b, nil,
                                         node(c, nil, nil)),
                                   node(d, nil, nil))), Ns).
      Ns = [b, c, a, d].
    

You can obtain other orders by moving the terminal `[Name]` in the DCG body.

## 3 Left Recursion
<a id='anch3' />

Conversely, given a sequence of node names, what are the trees that yield this sequence?:

      ?- phrase(tree_nodes(Tree), [a,b,c,d]).
      Tree = node(a, nil, node(b, nil, node(c, nil, node(d, nil, nil)))) ;
      (nontermination)

The system yields one (correct) solution, then loops. This is because the grammar is left-recursive: We recursively refer to a nonterminal `tree_nodes//1` before anything else. To be able to use this grammar for finding all matching trees, we need to encode that for the second rule to apply, at least one list element must be available since the rule contains exactly one terminal, and we need to check this in advance to avoid unbounded recursion. We can do this by introducing two additional arguments that let us limit the number of rule applications to the given list's length:

      tree_nodes(nil, Ls, Ls) --> [].
      tree_nodes(node(Name, Left, Right), [_|Ls0], Ls) -->
              tree_nodes(Left, Ls0, Ls1),
              [Name],
              tree_nodes(Right, Ls1, Ls).
    
Example:

      ?- Ns = [a,b,c,d], phrase(tree_nodes(Tree, Ns, _), Ns).
      Ns = [a, b, c, d],
      Tree = node(a, nil, node(b, nil, node(c, nil, node(d, nil, nil)))) ;
      Ns = [a, b, c, d],
      Tree = node(a, nil, node(b, nil, node(d, node(c, nil, nil), nil))) ;
      Ns = [a, b, c, d],
      Tree = node(a, nil, node(c, node(b, nil, nil), node(d, nil, nil))) ;
      etc.

## 4 Right-hand Context Notation
<a id='anch4' />

Using right-hand context notation, also called pushback lists, lets you insert list elements that were initially not in the list that is being parsed. A DCG rule of the form:

      Head, [T_1,...,T_n] --> Body.
    

can be read operationally as: parse the list using Body, then prepend the terms T\_1, ..., T\_n to the remaining list. For example:

      nt1, [b] --> [a].
      nt2      --> [b].
    
The body of `nt1//0` describes a list whose single element is the atom 'a'. Operationally, after `nt1//0` has consumed the atom 'a' in a list that is being parsed, it inserts the atom 'b' in front of the remaining list. `nt2//0` describes a list whose single element is the atom 'b'. The following query therefore succeeds, since `nt2//0` consumes the atom 'b' that is left in the list after `nt1//0` succeeds:

       ?- phrase((nt1,nt2), [a]).
       true.
    

We can also use `nt1//0` in isolation. However, the following query fails since `phrase/2` only succeeds if all list elements are consumed by the given DCG body:

      ?- phrase(nt1, [a]).
      false.
    

The difference list version `phrase/3` shows what remains after `nt1//0` succeeds:

      ?- phrase(nt1, [a], Rest).
      Rest = [b].
    

As expected, the atom 'b' remains in the list.

Using right-hand context notation, we can implement look ahead, which lets us inspect the next element in the list without removing it. Operationally, we first remove it and then push it back:

      look_ahead(T), [T] --> [T].
    

Example:

      ?- phrase(look_ahead(T), [a], Rest).
      T = a,
      Rest = [a].
    
## 5 Implicitly Passing States Around
<a id='anch5' />

Right-hand context notation is also useful to implicitly pass around a state representation that is only accessed and changed by a subset of rules. For example, let us count the leaves in a binary tree with the above presentation. The _state_ we shall pass around is a single number denoting the number of leaves encountered so far. To increment the state, we use Prolog's built-in arithmetic. To execute a regular Prolog predicate from within a DCG body, we use the DCG language construct `{}//1`. Operationally, when the construct `{Goal}` is executed in a DCG body, Goal is executed as a regular Prolog goal. Since a DCG must always describe a list, we wrap the state into a list and thus describe a list containing a single element. Notice that the second rule makes no reference at all to the state, since the number of leaves is not modified when an inner node is processed:

      num_leaves(nil), [N1] --> [N0], { N1 is N0 + 1 }.
      num_leaves(node(_,Left,Right)) -->
              num_leaves(Left),
              num_leaves(Right).

Example query, where the initial state is sensibly specified as 0, and the number of leaves is given by the remaining list element after num_leaves//1 succeeds:

      ?- phrase(num_leaves(node(a,node(b,nil,nil),
                                  node(c,nil,
                                          node(d,nil,nil)))), [0], [N]).
      N = 5.
    

## 6 Parsing From Files
<a id='anch6' />

In SWI-Prolog, DCGs can be transparently applied to files using [`library(pio)`](http://www.swi-prolog.org/pldoc/doc_for?object=section%282,%27A.19%27,swi%28%27/doc/Manual/pio.html%27%29%29).

Consider for example the following DCG that describes a list of character codes:

       like(What) --> "I like ", list(What), ".", list(_).

       list([]) --> [].
       list([L|Ls]) --> [L], list(Ls).

We can use this DCG to parse a given string, which is a list of character codes:

      ?- phrase(like(What), "I like it. The rest is ignored").
      What = [105, 116] ;
      false.
    
As expected, What is unified with the character codes for i and t.

Using `library(pio)`, we can transparently parse from a file with the same DCG. Assume that the file 'like.txt' starts with the string "I like it."

      ?- [library(pio)].
      true.

      ?- phrase_from_file(like(What), 'like.txt').
      What = [105, 116] ;
      false.

Again, What is unified with the character codes for _i_ and _t_. 

## 7 Implementation
<a id='anch7' />

To see how DCGs are internally implemented in SWI-Prolog, you can use `listing//1`. For example, to see the actual source code for `num_leaves//1`:

      ?- listing(num_leaves//1).
      num_leaves(nil, A, D) :-
           A=[B|C],
           E is B+1,
           F=C,
           D=[E|F].
      num_leaves(node(_, A, C), B, E) :-
           num_leaves(A, B, D),
           num_leaves(C, D, E).
    

We see that internally, the two DCG rules of `num_leaves//1` were translated into regular Prolog rules with two additional arguments, following mechanical rewriting steps. The translation of DCGs to regular Prolog code is done by `term_expansion/2`, a mechanism analogous to macros in other languages.

For portability, it is best not to rely on a particular expansion method, and instead to stick to regular DCG constructs like right-hand context notation to refer to states and the `phrase/2` interface to execute a DCG. 

## 8 Using DCGs
<a id='anch8' />

Consider using DCGs if you are:

 * describing a list and your code is more complicated than you think it could be.
 * parsing a list.
 * reading from a file.
 * passing around a state representation that only a few predicates actually use or modify.

## Conclusion
<a id='anchconclusion' />

Thanks for taking this tutorial. If I can improve anything please email me at aogborn (hat) uh.edu.

If you make something beautiful, drop us a link.

Thanks,

Annie
Markus




