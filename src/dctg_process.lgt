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


:- object(dctg,
	imports([dctg_print_tree, dctg_translate])).

	:- info([
		version is 1:0:0,
		author is 'Lindsey Spratt',
		date is 2022-2-14,
		comment is 'Definite Clause Translation Grammar (DCTG), based on the work of Harvey Abramson.'
	]).

	:- public(process/2).
	:- mode(process(+evaluable, -clause), one).
	:- info(process/2, [
		comment is 'Process a Definite Clause Translation Grammar expression to create a Prolog clause that implements the expression.',
		argnames is ['DCTGExpression', 'Clause']
	]).

	:- public(consult/2).
	:- mode(consult(+file_path, +atom), one).
	:- info(consult/2, [
		comment is 'Create a ``GrammarObject`` from the Definite Clause Grammar expressions in the given ``File``.',
		argnames is ['File', 'GrammarObject']
	]).

	:- public(sentence/1).
	:- mode(sentence(+list), one).
	:- info(sentence/1, [
		comment is 'Process the ``List`` using the DCTG predicate ``sentenceDCTG/3`` (this presumes having processed a DCTG expression with the name "sentence").',
		argnames is ['List']
	]).

	:- private(tidy/2).
	:- mode(tidy(+term, -term), one).
	:- info(tidy/2, [
		comment is 'Tidy (simplify) the ``InputExpression`` to create the ``OutputExpression``.',
		argnames is ['InputExpression', 'OutputExpression']
	]).

	/*
	This Logtalk implementation of the Definite Clause Translation Grammar is
	based on an implementation created by Lindsey Spratt in December, 1986,
	which in turn was based on "Definite Clause Translation Grammars" by
	Harvey Abramson in Proceedings of the Logic Programming Symposium,
	IEEE, Atlantic City, New Jersey, February 1984.
	*/

	:- uses(list, [
		append/3, length/2, member/2, reverse/2
	]).

	:- uses(logtalk, [
		print_message(debug, dctg, Message) as dbg(Message)
	]).

	:- include(operators).

	process((LP::=[]<:>Sem),H) :-
		!,
		^^t_lp(LP, [], S, S, Sem, H).
	process((LP::=[]), H) :-
		!,
		^^t_lp(LP, [], S, S, true, H).
	process((LP::=RP<:>Sem), (H:-B)) :-
		!,
		dbg('  Process 3: ~w'+[rp(RP)]),
		^^t_rp(RP, [], StL, S, SR, B1),
		reverse(StL, RStL),
		dbg('  Process 3: ~w'+[lp(LP, RStL, S, SR)]),
		^^t_lp(LP, RStL, S, SR, Sem, H),
		tidy(B1, B).
	process((LP::=RP), (H:-B)) :-
		!,
		process((LP::=RP<:>true), (H:-B)).
	process(end_of_file, _) :- !, fail.
	process(Clause, Clause).
%	process(ClauseIN, ClauseOUT) :-
%		expand_dcg(ClauseIN, ClauseOUT).

	consult(File, GrammarObject) :-
		open(File, read, S),
		dctg_consume(S, Clauses),
		close(S),
		define_grammar(GrammarObject, Clauses).

	dctg_consume(S, [Y|Clauses]) :-
		read(S, X),
		dbg('Consuming ~w'+[X]),
		process(X,Y),
		!,
		dctg_consume(S, Clauses).
	dctg_consume(_, []).

	define_grammar(GrammarObject, Clauses) :-
		(	member(dctg_main(Main, Eval), Clauses) ->
			true
		;	context(Context),
			throw(error(no_dctg_main_error(GrammarObject), Context))
		),
		grammar_main_predicate(Main, Eval, ParseIndicator, ParseClause, EvaluateIndicator, EvaluateClause),
		(	current_object(GrammarObject) ->
			abolish_object(GrammarObject)
		;	true
		),
		SemClause = (^^(A,B) :- ::eval(A, B)),
		create_object(
			GrammarObject,
			[imports(dctg_evaluate)],
			[public(ParseIndicator), public(EvaluateIndicator), public((^^)/2)],
			[ParseClause, EvaluateClause, SemClause| Clauses]
		).

	/*
	Create
	parse(Source, ExtraArgs, Tree) :-
		MainDCTG(ExtraArgs, Tree, Source, []).
	evaluate(Source, ExtraArgs, ResultArgs) :-
		MainDCTG(ExtraArgs, Tree, Source, []),
		Tree ^^ MainEvaluate(ResultArgs).
	where ExtraArgs and ResultArgs are sequences of 0, 1, or more terms,
	Main, ExtraArgs length, MainEvaluate, and ResultArgs length are specified by dctg_main/2 fact.
	*/
	grammar_main_predicate(Main/ExtraArgCount, SemFunctor/SemArgCount,
			parse/ParseArity, ParseClause,
			evaluate/EvalArity, EvaluateClause) :-
		atom_concat(Main, 'DCTG', MainFunctor),
		length(ExtraArgs, ExtraArgCount),
		MainPrefix = [MainFunctor| ExtraArgs],
		append(MainPrefix, [Tree, Source, []], MainList),
		MainHead =.. MainList,
		ParsePrefix = [parse, Source| ExtraArgs],
		append(ParsePrefix, [Tree], ParseList),
		ParseHead =.. ParseList,
		functor(ParseHead, _, ParseArity),
		ParseClause = (ParseHead :- MainHead),
		length(SemArgs, SemArgCount),
		SemHead =.. [SemFunctor|SemArgs],
		EvalPrefix = [evaluate, Source| ExtraArgs],
		append(EvalPrefix, SemArgs, EvalList),
		EvaluateHead =.. EvalList,
		functor(EvaluateHead, _, EvalArity),
		EvaluateClause = (EvaluateHead :- MainHead, ::eval(Tree, SemHead)).

/*	sentence(Source) :-
		sentenceDCTG(T, Source, []),
		::dctg_print_tree(T),
		T ^^ logic(Proposition),
		nl,
		write(Proposition).
*/
	A ^^ B :-
		::eval(A, B).

	% auxiliary predicates

	tidy(((P1, P2), P3), Q) :-
		!,
		tidy((P1, (P2, P3)), Q).
	tidy((P1, P2), (Q1, Q2)) :-
		!,
		tidy(P1, Q1),
		tidy(P2, Q2).
	tidy(A, A).

:- end_object.
