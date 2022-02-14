:- object(utilities).
	:- info([
		version is 1:0:0,
		author is 'Lindsey Spratt',
		date is 2022-2-14,
		comment is 'DCTG utilities.'
	]).

	:- public(add_extra_args/3).
	:- mode(add_extra_args(+list, +evaluable, -evaluable), one).
	:- info(add_extra_args/3, [
		comment is 'Extend ``BaseStructure`` functor with "DCTG" and create ``ExtendedStructure`` with extended functor and the concatenation of the base arguments and the ``NewArguments``.',
		argnames is ['NewArguments', 'BaseStructure', 'ExtendedStructure']
	]).

	:- public(tidy/2).
	:- mode(tidy(+term, -term), one).
	:- info(tidy/2, [
		comment is 'Tidy (simplify) the ``InputExpression`` to create the ``OutputExpression``.',
		argnames is ['InputExpression', 'OutputExpression']
	]).

	:- public(char/2).

	:- public(tab/1).
	:- mode(tab(+integer), one).
	:- info(tab/1, [
		comment is 'Write N spaces.',
		argnames is ['SpaceCount']
	]).

	:- public(writeseqnl/1).
	:- mode(writeseqnl(+list), one).
	:- info(writeseqnl/1, [
		comment is 'Write each element in the sequence ``List`` then write a newline.',
		argnames is ['List']
	]).

	:- uses(list, [
		append/3
	]).

	add_extra_args(L, T, T1) :-
		T =.. [F|Tlist],
		atom_concat(F, 'DCTG', Nf),
		append(Tlist, L, Tlist1),
		T1 =.. [Nf|Tlist1].

	tidy(((P1, P2), P3), Q) :-
		!,
		tidy((P1, (P2, P3)), Q).
	tidy((P1, P2), (Q1, Q2)) :-
		!,
		tidy(P1, Q1),
		tidy(P2, Q2).
	tidy(A, A).

	tab(0) :- !.
	tab(N) :-
		write(' '),
		K is N-1,
		tab(K).

	writeseqnl([]) :- nl.
	writeseqnl([H|T]) :-
		write(H),
		writeseqnl(T).

	dctg_inputDCTG(Input, _, Input, _).

	dctg_input_tailDCTG(Tail, _, _, Tail).

:- end_object.
