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


:- category(dctg_evaluate,
	extends(dctg_print_tree)).

	:- info([
		version is 1:0:0,
		author is 'Lindsey Spratt',
		date is 2022-02-14,
		comment is 'DCTG evaluate semantics.'
	]).

	:- public((^^)/2).
	:- mode(^^(+term, +term), one_or_more).
	:- info((^^)/2, [
		comment is 'The ^^ predicate evaluates the DCTG semantic Goals to with respect to the DCTG Tree semantics.',
		argnames is ['Tree', 'Goals']
	]).

	:- public((eval)/2).
	:- mode(eval(+term, +term), one_or_more).
	:- info((eval)/2, [
		comment is 'The eval predicate evaluates the DCTG semantic Goals to with respect to the DCTG Tree semantics.',
		argnames is ['Tree', 'Goals']
	]).

	:- public(trace/1).

	:- public(untrace/1).

	:- public(notrace/0).

	:- public(notrace/1).

	:- private(trace_/1).
	:- dynamic(trace_/1).

	:- private(notrace_/1).
	:- dynamic(notrace_/1).

	:- uses(type, [
		check/3
	]).

	:- include(dctg_operators).

	^^(Node, Args) :- eval(Node, Args).

	eval(node(Name, _, Sem), Args) :-
		trace_node_message('call', 'fail', Name, Sem, Args),
		context(Context),
		check(nonvar, Sem, Context),
		dctg_eval(Sem, Args), % Sem ^^ Args
		trace_node_message('exit', 'redo', Name, Sem, Args).

	:- meta_predicate(dctg_eval(*,*)).

	dctg_eval(((Args ::- Traverse), _Rules), Args) :-
		!,
		trace_message('call', 'fail', Args, Traverse),
		call(Traverse),
		trace_message('exit', 'redo', Args, Traverse).
	dctg_eval((Args, _Rules), Args) :- !.
	dctg_eval((_, Rules), Args) :-
		dctg_eval(Rules, Args).
	dctg_eval((Args ::- Traverse), Args) :-
		trace_message('call', 'fail', Args, Traverse),
		call(Traverse),
		trace_message('exit', 'redo', Args, Traverse).
	dctg_eval(Args, Args).

	/*
	((Args ::- Traverse), Rules) ^^ Args :-
	!,
	trace_message('call','fail',Args,Traverse),
	call(Traverse),
	trace_message('exit','redo',Args,Traverse).
	(Args, Rules) ^^ Args :- !.
	(_, Rules) ^^ Args :-
	Rules ^^ Args.
	(Args ::- Traverse) ^^ Args :-
	trace_message('call','fail', Args, Traverse),
	call(Traverse),
	trace_message('exit','redo', Args, Traverse).
	Args ^^ Args.
	*/

	trace_node_message(Success, Failure, Name, Sem, Args) :-
		traced_attachment(Args)
			-> (	nl,
				write(Success), write(' '),
				trace_node_message1(Name, Sem, Args)
			;
				nl,
				write(Failure), write(' '),
				trace_node_message1(Name, Sem, Args),
				!,
				fail
			 )
		; true.


	trace_node_message1(Name, Sem, Args) :-
		write('attachment: '),
		writeq(Args),
		nl,
		write('on node: '),
		^^print_node(Name, Sem).


	trace_message(_Success,_Failure,Args,_Body) :-
		\+ traced_attachment(Args),
		!.
	trace_message(Success,_Failure,_Args,Body) :-
		write(Success), write(' '),
		^^print_semantics(Body),
		nl.
	trace_message(_Success, Failure, _Args,Body) :-
		write(Failure), write(' '),
		^^print_semantics(Body),
		nl,
		!,
		fail.

	traced_attachment(Args) :-
		\+ \+ trace_(Args),
		\+ notrace_(Args).

	trace(F) :-
		assertz(trace_(F)),
		retractall(notrace_(F)).

	untrace(F) :-
		retractall(trace_(F)).

	notrace :-
		retractall(trace_(_)).

	notrace(F) :-
		assertz(notrace_(F)),
		retractall(trace_(F)).

:- end_category.
