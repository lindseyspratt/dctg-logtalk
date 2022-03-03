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
	implements(expanding),
	imports([dctg_print_tree, dctg_translate])).

	:- info([
		version is 1:0:0,
		author is 'Lindsey Spratt',
		date is 2022-02-24,
		comment is 'Definite Clause Translation Grammar (DCTG), based on the work of Harvey Abramson.'
	]).

	:- public(process/2).
	:- mode(process(+evaluable, -clause), one).
	:- info(process/2, [
		comment is 'Process a Definite Clause Translation Grammar expression to create a Prolog clause that implements the expression.',
		argnames is ['DCTGExpression', 'Clause']
	]).

	:- public(consult/2).
	:- mode(consult(+atom, -atom), one).
	:- info(consult/2, [
		comment is 'Consults a Definite Clause Translation Grammar file (which must have a ``.dctg`` extension) by creating and loading a Logalk file defining a ``GrammarObject`` object named after the file.',
		argnames is ['File', 'GrammarObject']
	]).

	:- public(consult/1).
	:- mode(consult(+atom), one).
	:- info(consult/1, [
		comment is 'Consults a Definite Clause Translation Grammar file (which must have a ``.dctg`` extension) by creating and loading a Logalk file defining a ``GrammarObject`` object named after the file.',
		argnames is ['File']
	]).

	:- public(compile/1).
	:- mode(compile(+atom), one).
	:- info(compile/1, [
		comment is 'Compiles a Definite Clause Translation Grammar file (which must have a ``.dctg`` extension) to a Logalk file written to the same directory and defining a ``GrammarObject`` object named after the file.',
		argnames is ['File']
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

	:- public(grammar_main_predicate/10).

	/*
	This Logtalk implementation of the Definite Clause Translation Grammar is
	based on an implementation created by Lindsey Spratt in December, 1986,
	which in turn was based on "Definite Clause Translation Grammars" by
	Harvey Abramson in Proceedings of the Logic Programming Symposium,
	IEEE, Atlantic City, New Jersey, February 1984.
	*/

	:- uses(format, [format/2]).
	:- uses(list, [append/3, length/2, reverse/2]).
	:- uses(logtalk, [print_message(debug, dctg, Message) as dbg(Message)]).
	:- uses(os, [decompose_file_name/4]).
	:- uses(user, [atomic_list_concat/2]).

	:- include(dctg_operators).

	process(DCTGExpression, Clause) :-
		term_expansion(DCTGExpression, Clause).

	consult(Path, Name) :-
		decompose_file_name(Path, _, Name, '.dctg'),
		this(This),
		logtalk_load(Path, [hook(This)]).

	consult(Path) :-
		consult(Path, _).

	compile(Path) :-
		decompose_file_name(Path, Directory, Name, '.dctg'),
		atomic_list_concat([Directory, Name, '.lgt'], Output),
		open(Output, write, Stream),
		this(This),
		logtalk_compile(Path, [hook(hook_pipeline([This,write_to_stream_hook(Stream,[quoted(true)])]))]),
		close(Stream).

	term_expansion(Term, _) :-
		dbg('Consuming ~q'+[Term]),
		fail.
	term_expansion(begin_of_file, [(:- object(Object, imports(dctg_evaluate)))]) :-
		logtalk_load_context(basename, Basename),
		atom_concat(Object, '.dctg', Basename).
	term_expansion((:- Directive0), [(:- Directive)]) :-
		nonvar(Directive0),
		Directive0 =.. [object, Object| Relations0],
		add_category_import(Relations0, Relations),
		Directive =.. [object, Object| Relations].
	term_expansion((:- Directive0), [(:- Directive)]) :-
		nonvar(Directive0),
		Directive0 =.. [category, Object| Relations0],
		add_category_extend(Relations0, Relations),
		Directive =.. [category, Object| Relations].
	term_expansion(dctg_main(Main, Eval),
		[ParseDirective1, ParseDirective2, EvaluateDirective1, EvaluateDirective2,
		 ParseClause1, ParseClause2, EvaluateClause1, EvaluateClause2, SemClause]) :-
		grammar_main_predicate(Main, Eval,
			ParseIndicator1, ParseClause1,
			ParseIndicator2, ParseClause2,
			EvaluateIndicator1, EvaluateClause1,
			EvaluateIndicator2, EvaluateClause2
			),
		dbg('~w -> ~w~n'+[dctg_main(Main, Eval), grammar_main_predicate(Main, Eval,
			ParseIndicator1, ParseClause1,
			ParseIndicator2, ParseClause2,
			EvaluateIndicator1, EvaluateClause1,
			EvaluateIndicator2, EvaluateClause2
		)]),
		ParseDirective1 = (:- public(ParseIndicator1)),
		ParseDirective2 = (:- public(ParseIndicator2)),
		EvaluateDirective1 = (:- public(EvaluateIndicator1)),
		EvaluateDirective2 = (:- public(EvaluateIndicator2)),
		SemClause = (^^(A,B) :- ::eval(A, B)).
	term_expansion((LP::=[]<:>Sem), [H|Clauses]) :-
		!,
		^^t_lp(LP, [], S, S, Sem, H, Clauses).
	term_expansion((LP::=[]), H) :-
		!,
		^^t_lp(LP, [], S, S, true, H, []).
	term_expansion((LP::=RP<:>Sem), [(:-discontiguous(F/A)),(H:-B)|Clauses]) :-
		!,
		dbg('  Process 3: ~w'+[rp(RP)]),
		^^t_rp(RP, [], StL, S, SR, B1),
		reverse(StL, RStL),
		dbg('  Process 3: ~w'+[lp(LP, RStL, S, SR)]),
		^^t_lp(LP, RStL, S, SR, Sem, H, Clauses),
		functor(H, F, A),
		tidy(B1, B).
	term_expansion((LP::=RP), Result) :-
		!,
		term_expansion((LP::=RP<:>true), Result).
	term_expansion(end_of_file, [(:- end_object), end_of_file]) :-
		logtalk_load_context(basename, Basename),
		atom_concat(_, '.dctg', Basename).

	add_category_import([], [imports(dctg_evaluate)]).
	add_category_import([imports(Imports0)| Relations], [imports(Imports)| Relations]) :-
		!,
		add_entity(Imports0, Imports).
	add_category_import([Relation0| Relations0], [Relation0| Relations]) :-
		add_category_import(Relations0, Relations).

	add_category_extend([], [extends(dctg_evaluate)]).
	add_category_extend([extends(Extends0)| Relations], [imports(Extends)| Relations]) :-
		!,
		add_entity(Extends0, Extends).
	add_category_extend([Relation0| Relations0], [Relation0| Relations]) :-
		add_category_extend(Relations0, Relations).

	add_entity([Entity0| Entities0], [Entity0| Entities]) :-
		!,
		add_entity(Entities0, Entities).
	add_entity([], [dctg_evaluate]) :-
		!.
	add_entity((Entity0, Entities0), (Entity0, Entities)) :-
		!,
		add_entity(Entities0, Entities).
	add_entity(Entity, (Entity, dctg_evaluate)).

	/*
	Create
	parse(Source, ExtraArgs, Tree) :-
		MainDCTG(ExtraArgs, Tree, Source, []).
	parse(Source, Remainder, ExtraArgs, Tree) :-
		MainDCTG(ExtraArgs, Tree, Source, Remainder).
	evaluate(Source, ExtraArgs, ResultArgs) :-
		MainDCTG(ExtraArgs, Tree, Source, []),
		Tree ^^ MainEvaluate(ResultArgs).
	evaluate(Source, Remainder, ExtraArgs, ResultArgs) :-
		MainDCTG(ExtraArgs, Tree, Source, Remainder),
		Tree ^^ MainEvaluate(ResultArgs).
	where ExtraArgs and ResultArgs are sequences of 0, 1, or more terms,
	Main, ExtraArgs length, MainEvaluate, and ResultArgs length are specified by dctg_main/2 fact.
	*/
	grammar_main_predicate(Main/ExtraArgCount, SemInfo,
			parse/ParseArity1, ParseClause1,
			parse/ParseArity2, ParseClause2,
			evaluate/EvalArity1, EvalClause1,
			evaluate/EvalArity2, EvalClause2
			) :-
		atom_concat(Main, 'DCTG', MainFunctor),
		MainInfo = MainFunctor/ExtraArgCount,
		ParsePrefix1 = [parse, Source],
		ParsePrefix2 = [parse, Source, Remainder],
		grammar_parse_predicate(MainInfo, Source, [], ParsePrefix1, ParseClause1, ParseArity1),
		grammar_parse_predicate(MainInfo, Source, Remainder, ParsePrefix2, ParseClause2, ParseArity2),

		EvalPrefix1 = [evaluate, Source],
		EvalPrefix2 = [evaluate, Source, Remainder],
		grammar_evaluate_predicate(MainInfo, Source, [], EvalPrefix1, SemInfo, EvalClause1, EvalArity1),
		grammar_evaluate_predicate(MainInfo, Source, Remainder, EvalPrefix2, SemInfo, EvalClause2, EvalArity2).

	grammar_parse_predicate(MainFunctor/ExtraArgCount, Source, Remainder, ParsePrefix, ParseClause, ParseArity) :-
		length(ExtraArgs, ExtraArgCount),
		grammar_main_head(MainFunctor, ExtraArgs, Tree, Source, Remainder, MainHead),
		grammar_clause_head(ParsePrefix, ExtraArgs, [Tree], ParseHead, ParseArity),
		ParseClause = (ParseHead :- MainHead).

	grammar_evaluate_predicate(MainFunctor/ExtraArgCount, Source, Remainder, EvalPrefix, SemFunctor/SemArgCount, EvalClause, EvalArity) :-
		length(ExtraArgs, ExtraArgCount),
		grammar_main_head(MainFunctor, ExtraArgs, Tree, Source, Remainder, MainHead),
		length(SemArgs, SemArgCount),
		SemHead =.. [SemFunctor|SemArgs],
		grammar_clause_head(EvalPrefix, ExtraArgs, SemArgs, EvalHead, EvalArity),
		EvalClause = (EvalHead :- MainHead, ::eval(Tree, SemHead)).

	grammar_main_head(MainFunctor, ExtraArgs, Tree, Source, Remainder, MainHead) :-
		MainPrefix = [MainFunctor| ExtraArgs],
		append(MainPrefix, [Tree, Source, Remainder], MainList),
		MainHead =.. MainList.

	grammar_clause_head(HeadPrefix, ExtraArgs, FinalArgs, Head, Arity) :-
		append(HeadPrefix, ExtraArgs, ExtendedPrefix),
		append(ExtendedPrefix, FinalArgs, HeadList),
		Head =.. HeadList,
		functor(Head, _, Arity).

/*	sentence(Source) :-
		sentenceDCTG(T, Source, []),
		::dctg_print_tree(T),
		T ^^ logic(Proposition),
		nl,
		write(Proposition).
	A ^^ B :-
		::eval(A, B).
*/

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
