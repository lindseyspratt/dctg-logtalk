:- category(print_tree).

	:- info([
		version is 1:0:0,
		author is 'Lindsey Spratt',
		date is 2022-2-12,
		comment is 'DCTG Print Tree.'
	]).

	:- public(print_tree/1).
	:- mode(print_tree(+tree), one).
	:- info(print_tree/1, [
		comment is 'Print a DCTG tree.',
		argnames is ['DCTGTree']
	]).

	:- public(print_node/2).
	:- mode(print_node(+term, +term), one).
	:- info(print_node/2, [
		comment is 'Print Name and Semantics from a DCTG node.',
		argnames is ['Name', 'Semantics']
	]).

	:- public(print_semantics/1).
	:- mode(print_semantics(+term), one).
	:- info(print_semantics/1, [
		comment is 'Print DCTGSemantics from a DCTG tree node.',
		argnames is ['DCTGSemantics']
	]).
	
	:- op(1001,xfy,'...').
	:- op(1150, xfx, '::=').
	:- op(1175, xfx, <:>).
	:- op(1150, xfx, ::-).
	:- op(650, yfx, ^^).
	:- op(1120, xfx, <<+).
	:- op(1110, xfx, +>>).

	print_tree(Tree) :-
		print_tree(Tree, 0).
		
	print_tree(node(Name, Children, Sem), Indent) :-
		!,
		utilities::tab(Indent),
		print_node(Name, Sem),
		NextIndent is Indent + 2,
		print_children(Children, NextIndent).
		print_tree(Terminal, Indent) :-
		utilities::tab(Indent),
		write(Terminal), nl.

	print_node(Name, Sem) :-
		functor(Name, F, N),
		write(F),
		(N = 0 -> true ; write('/'), write(N)),
		write('['),
		print_semantics(Sem),
		write(']'),
		nl.

	print_semantics((Rule, OtherRules)) :-
		!,
		print_rule(Rule),
		utilities::tab(1),
		print_semantics(OtherRules).
		print_semantics(Rule) :-
		print_rule(Rule).

	print_rule((Head ::- _)) :-
		!,
		functor(Head, Functor, _),
		%Head =.. [Functor|_],
		write(Functor).
	print_rule(Rule) :-
		functor(Rule, Functor, _),
		%Rule =.. [Functor|_],
		write(Functor).

	print_children([], _) :- !.
	print_children([Node|OtherNodes], Indent) :-
		!,
		print_tree(Node, Indent),
		print_children(OtherNodes, Indent).
	print_children(X, _I) :-
		functor(X, F, N),
		utilities::writeseqnl(['Unable to print term with functor "', F, '"and arity', N, '.']),
		throw(error(type_error(node, X), print_children/2)).
	

:- end_category.
