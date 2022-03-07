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


:- category(dctg_translate).

	:- info([
		version is 1:0:0,
		author is 'Lindsey Spratt',
		date is 2022-03-02,
		comment is 'DCTG expression translation.'
	]).

	:- protected(t_lp/7).
	:- mode(t_lp(+expression, -list, -term, -term, -term, -callable, -list), one).
	:- info(t_lp/7, [
		comment is 'Translate the left-hand side of a DCTG expression.',
		argnames is ['ExpressionHead', 'StL', 'S', 'SR', 'Sem', 'ClauseHead', 'AuxiliaryClauses']
	]).

	:- protected(t_rp/6).
	:- mode(t_rp(+expression, -list, -list, -term, -term, -callable), one).
	:- info(t_rp/6, [
		comment is 'Translate the right-hand side of a DCTG expression.',
		argnames is ['ExpressionBody', 'St', 'StR', 'S', 'SR', 'Goal']
	]).

	:- private(add_extra_args/3).
	:- mode(add_extra_args(+list, +callable, -callable), one).
	:- info(add_extra_args/3, [
		comment is 'Extend ``BaseStructure`` functor with "DCTG" and create ``ExtendedStructure`` with extended functor and the concatenation of the base arguments and the ``NewArguments``.',
		argnames is ['NewArguments', 'BaseStructure', 'ExtendedStructure']
	]).

	:- private(clause_id_/1).
	:- dynamic(clause_id_/1).

	:- uses(list, [append/3]).
	:- uses(logtalk, [print_message(debug, dctg, Message) as dbg(Message)]).
	:- uses(type, [check/3]).
	:- uses(user, [atomic_concat/3]).

	:- include(dctg_operators).

	t_lp((LP,List), StL, S, SR, Sem, H, Clauses) :-
		!,
		context(Context),
		check(list, List, Context),
		append(List, SR, List2),
		t_clause_id(ID),
		phrase(t_sem(Sem, ID, TSem), Clauses),
		add_extra_args([node(LP, StL, TSem), S, List2], LP, H).
	t_lp(LP, StL, S, SR, Sem, H, Clauses) :-
		t_clause_id(ID),
		phrase(t_sem(Sem, ID, TSem), Clauses),
		add_extra_args([node(LP, StL, TSem), S, SR], LP, H).

	t_rp(!, St, St, S, S, !) :-
		!.
	t_rp([], St, [[]|St], S, S1, S=S1) :-
		!.
	t_rp([X], St, [[NX]|St], S, SR, S = [X| SR]) :-
		dbg('    t_rp 3: ~w'+[rp([X], St)]),
		integer(X),
		char_code(NX, X),
		!.
	t_rp([X], St, [X|St], S, SR, S = [X| SR]) :-
		dbg('    t_rp 4: ~w'+[rp([X], St)]),
		!.
	t_rp([X|R], St, [[NX|NR]|St], S, SR, (S = [X| SR1], RB)) :-
		integer(X),
		char_code(NX, X),
		!,
		context(Context),
		check(list_or_partial_list, R, Context),
		t_rp(R, St, [NR|St], SR1, SR, RB).
	t_rp([X|R], St, [[X|NR]|St], S, SR, (S = [X| SR1], RB)) :-
		!,
		context(Context),
		check(list_or_partial_list, R, Context),
		t_rp(R, St, [NR|St], SR1, SR, RB).
	t_rp({T}, St, St, S, S, T) :-
		!,
		context(Context),
		check(callable, T, Context).
	t_rp((T,R), St, StR, S, SR, (Tt,Rt)) :-
		!,
		dbg('    t_rp 8a: ~w'+[rp(T, St, S)]),
		t_rp(T, St, St1, S, SR1, Tt),
		dbg('    t_rp 8b: ~w'+[rp(T, St1, SR1)]),
		t_rp(R, St1, StR, SR1, SR, Rt).
	t_rp((T;R), St, StR, S, SR,
			(StR=St1, SR=SR1, Tt;StR=St2, SR=SR2, Rt)) :-
		!,
		t_rp(T, St, St1, S, SR1, Tt),
		t_rp(R, St, St2, S, SR2, Rt).
	t_rp(T^^N, St, [N|St], S, SR, Tt) :-
		!,
		context(Context),
		check(callable, T, Context),
		add_extra_args([N, S, SR], T, Tt).
	t_rp(T, St, [St1|St], S, SR, Tt) :-
		context(Context),
		check(callable, T, Context),
		add_extra_args([St1, S, SR], T, Tt).

	% auxiliary predicates

	% Semantics translation converts (a ::- X^^b, Y^^c, d) to
	% (a ::- dctg_sem(1, a, [X,Y])) and (dctg_sem(1, a, [X,Y]) :- X^^b, Y^^c, d).
	% The X and Y are node variables that refer to nonterminal nodes in the DCTG clause being translated.
	% The clause is compiled as Logtalk source in the current dctg object being compiled.

	t_sem((A,B), ID, (ASem,BSem)) -->
		!,
		t_sem(A, ID, ASem),
		t_sem(B, ID, BSem).
	t_sem((H ::- B), ID, Sem) -->
		!,
		[CH1, CH2, CH3],
		{t_sem_body(H, B, ID, Sem, [CH1, CH2, CH3])}.
	t_sem(H, _, H) -->
		[].

	t_sem_body(H, B, ClauseID, (H ::- ::DCTGSem), [(:- private(Functor/2)), (:- meta_predicate(MetaPred)), (DCTGSem :- B)]) :-
		atomic_concat(dctg_sem, ClauseID, Functor),
		MetaPred =.. [Functor,(*),(*)],
		DCTGSem =.. [Functor, H, Nodes],
		phrase(find_nodes(B), NodesRaw),
		% NodesRaw may have duplicate references to nodes, sort uniquifies the list.
		sort(NodesRaw, Nodes),
		format('Compiled ~w to ~w and ~w~n', [(H ::- B), (H ::- ::DCTGSem), (DCTGSem :- B)]).

	t_clause_id(New) :-
		(	retract(clause_id_(Old))
		->	New is Old + 1
		;	New = 1
		),
		assertz(clause_id_(New)).

	find_nodes(Var) -->
		{var(Var)},
		!,
		[Var].
	find_nodes((A,B)) -->
		!,
		find_nodes(A),
		find_nodes(B).
	find_nodes(Node^^_) -->
		!,
		[Node].
	find_nodes(B) -->
		{B =.. [_| Args]},
		find_nodes_list(Args).

	find_nodes_list([]) -->
		[].
	find_nodes_list([Head| Tail]) -->
		find_nodes(Head),
		find_nodes_list(Tail).

	add_extra_args(L, T, T1) :-
		T =.. [F|Tlist],
		atom_concat(F, 'DCTG', Nf),
		append(Tlist, L, Tlist1),
		T1 =.. [Nf|Tlist1].

:- end_category.
