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


:- set_prolog_flag(double_quotes, codes).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:5:0,
		date is 2022-03-03,
		author is 'Lindsey Spratt',
		comment is 'Test cases for the DCTG translator.'
	]).

	:- include('../src/dctg_operators').

	% terminal tests with list notation

	:- uses(dctg, [term_expansion(A,_) as proc(A)]).

	test(dctg_terminal_list_01, true) :-
		proc((p ::= [])).
	test(dctg_terminal_list_02, true) :-
		proc((p ::= [b])).
	test(dctg_terminal_list_03, true) :-
		proc((p ::= [abc, xyz])).
	test(dctg_terminal_list_04, error(_)) :-
		proc((p ::= [abc | xyz])).
	test(dctg_terminal_list_05, true) :-
		proc((p ::= [[], {}, 3, 3.2, a(b)])).
	test(dctg_terminal_list_06, true) :-
		proc((p ::= [_])).

	% terminal tests with string notation:
	test(dctg_terminal_string_01, true) :-
		proc((p ::= "b")).
	test(dctg_terminal_string_02, true) :-
		proc((p ::= "abc", "q")).
	test(dctg_terminal_string_03, true) :-
		proc((p ::= "abc" ; "q")).

	% simple non-terminal tests:
	test(dctg_non_terminal_01, true) :-
		proc((p ::= b)).
	test(dctg_non_terminal_02, error(_)) :-
		proc((p ::= 3)).
	test(dctg_non_terminal_03, true) :-
		proc((p(X) ::= b(X))).

	% conjunction tests

	test(dctg_conjunction_01, true) :-
		proc((p ::= b, c)).
	test(dctg_conjunction_02, true) :-
		proc((p ::= true, c)).
	test(dctg_conjunction_03, true) :-
		proc((p ::= fail, c)).
	test(dctg_conjunction_04, true) :-
		proc((p(X) ::= call(X), c)).

	% disjunction tests

	test(dctg_disjunction_01, true) :-
		proc((p ::= b ; c)).
	test(dctg_disjunction_02, true) :-
		proc((p ::= q ; [])).
	test(dctg_disjunction_03, true) :-
		proc((p ::= [a] ; [b])).

	% if-then-else tests

	test(dctg_if_the_else_01, true) :-
		proc((p ::= b -> c)).
	test(dctg_if_the_else_02, true) :-
		proc((p ::= b -> c; d)).
	test(dctg_if_the_else_03, true) :-
		proc((p ::= b -> c1, c2 ; d)).
	test(dctg_if_the_else_04, true) :-
		proc((p ::= b -> c ; d1, d2)).
	test(dctg_if_the_else_05, true) :-
		proc((p ::= b1, b2 -> c ; d)).
	test(dctg_if_the_else_06, true) :-
		proc((p ::= [x] -> [] ; q)).

	% negation tests

	test(dctg_negation_01, true) :-
		proc((p ::= \+ b, c)).
	test(dctg_negation_02, true) :-
		proc((p ::= b, \+ c, d)).

	% cut tests

	test(dctg_cut_01, true) :-
		proc((p ::= !, [a])).
	test(dctg_cut_02, true) :-
		proc((p ::= b, !, c, d)).
	test(dctg_cut_03, true) :-
		proc((p ::= b, !, c ; d)).
	test(dctg_cut_04, true) :-
		proc((p ::= [a], !, {fail})).
	test(dctg_cut_05, true) :-
		proc((p(a), [X] ::= !, [X, a], q)).
	test(dctg_cut_06, true) :-
		proc((p ::= a, ! ; b)).

	% {}/1 tests

	test(dctg_bypass_01, true) :-
		proc((p ::= {b})).
	test(dctg_bypass_02, error(_)) :-
		proc((p ::= {3})).
	test(dctg_bypass_03, true) :-
		proc((p ::= {c,d})).
	test(dctg_bypass_04, true) :-
		proc((p ::= '{}'((c,d)))).
	test(dctg_bypass_05, true) :-
		proc((p ::= {a}, {b}, {c})).
	test(dctg_bypass_06, true) :-
		proc((p ::= {q} -> [a] ; [b])).
	test(dctg_bypass_07, true) :-
		proc((p ::= {q} -> [] ; b)).
	test(dctg_bypass_08, true) :-
		proc((p ::= [foo], {write(x)}, [bar])).
	test(dctg_bypass_09, true) :-
		proc((p ::= [foo], {write(hello)},{nl})).
	test(dctg_bypass_10, true) :-
		proc((p ::= [foo], {write(hello), nl})).

	% "metacall" tests

	test(dctg_metacall_01, true) :-
		proc((p ::= _)).
	test(dctg_metacall_02, true) :-
		proc((p(X) ::= X)).

	% non-terminals corresponding to "graphic" characters
	% or built-in operators/predicates

	test(dctg_graphic_01, true) :-
		proc(('[' ::= b, c)).
	test(dctg_graphic_02, true) :-
		proc(((=) ::= b, c)).

	% pushback tests

	test(dctg_push_back_list_01, true) :-
		proc((p, [t] ::= b, c)).
	test(dctg_push_back_list_002, true) :-
		proc((p, [t] ::= b, [t])).
	test(dctg_push_back_list_003, true) :-
		proc((p, [t] ::= b, [s, t])).
	test(dctg_push_back_list_004, true) :-
		proc((p, [t] ::= b, [s], [t])).
	test(dctg_push_back_list_005, true) :-
		proc((p(X), [X] ::= [X])).
	test(dctg_push_back_list_006, true) :-
		proc((p(X, Y), [X, Y] ::= [X, Y])).
	test(dctg_push_back_list_007, true) :-
		proc((p(a), [X] ::= !, [X, a], q)).
	test(dctg_push_back_list_008, true) :-
		proc((p, [a,b] ::= [foo], {write(hello), nl})).
	test(dctg_push_back_list_09, error(_)) :-
		proc((p, [t1], [t2] ::= b, c)).
	test(dctg_push_back_list_10, error(_)) :-
		proc((p, b ::= b)).
	test(dctg_push_back_list_11, error(_)) :-
		proc(([t], p ::= b)).
	test(dctg_push_back_list_12, error(_)) :-
		proc(([t1], p, [t2] ::= b)).

	% semantic test

	test(dctg_semantics_01, true) :-
		proc((a ::= [] <:> b)).
	test(dctg_semantics_02, true) :-
		proc((a ::= [] <:> b ::- c)).

	test(dctg_example_01, true(V == [a,b,c])) :-
		file_path('../examples/token.dctg', Path),
		dctg::load(Path, Object),
		Object::evaluate([a,b,c], V).
	test(dctg_example_02, variant(V, E)) :-
		file_path('../examples/logic.dctg', Path),
		dctg::load(Path, Object),
		E = exists(M, musician(M) & forall(S, (scientist(S) & hesitates(S)) => helps(M,S))),
		Object::evaluate([a,musician,helps,every,scientist,that,hesitates], V).
	test(dctg_example_03, true((R == [], V == [a,b,c]))) :-
		file_path('../examples/token.dctg', Path),
		dctg::load(Path, Object),
		Object::evaluate([a,b,c], R, V).

	% embedding tests

	test(dctg_embed_01, true(V == [a,b,c])) :-
		file_path('embedded.lgt', Path),
		logtalk_load(Path, [hook(dctg)]),
		% avoid linter warning on unknown object by using the {}/1 control construct
		{embedded::evaluate([a,b,c], V)}.
	test(dctg_embed_02, true((R == [], V == [a,b,c]))) :-
		file_path('embedded.lgt', Path),
		logtalk_load(Path, [hook(dctg)]),
		% avoid linter warning on unknown object by using the {}/1 control construct
		{embedded::evaluate([a,b,c], R, V)}.

	% auxiliary predicates

	file_path(File, Path) :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, File, Path).

:- end_object.
