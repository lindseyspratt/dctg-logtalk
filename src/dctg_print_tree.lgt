%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Copyright (c) 2022 Lindsey Spratt
%  SPDX-License-Identifier: MIT
%
%  Licensed under the MIT License (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      https://opensource.org/licenses/MIT
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(dctg_print_tree).

	:- info([
		version is 1:0:0,
		author is 'Lindsey Spratt',
		date is 2022-2-14,
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
		comment is 'Print ``Name`` and ``Semantics`` from a DCTG node.',
		argnames is ['Name', 'Semantics']
	]).

	:- public(print_semantics/1).
	:- mode(print_semantics(+term), one).
	:- info(print_semantics/1, [
		comment is 'Print ``DCTGSemantics`` from a DCTG tree node.',
		argnames is ['DCTGSemantics']
	]).

	:- include(dctg_operators).

	print_tree(Tree) :-
		print_tree(Tree, 0).

	print_tree(node(Name, Children, Sem), Indent) :-
		!,
		tab(Indent),
		print_node(Name, Sem),
		NextIndent is Indent + 2,
		print_children(Children, NextIndent).
		print_tree(Terminal, Indent) :-
		tab(Indent),
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
		tab(1),
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
		writeseqnl(['Unable to print term with functor "', F, '"and arity', N, '.']),
		throw(error(type_error(node, X), print_children/2)).

	% auxiliary predicates

	tab(0) :- !.
	tab(N) :-
		write(' '),
		K is N-1,
		tab(K).

	writeseqnl([]) :- nl.
	writeseqnl([H|T]) :-
		write(H),
		writeseqnl(T).

:- end_category.
