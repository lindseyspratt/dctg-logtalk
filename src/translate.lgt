:- category(translate).

	:- info([
		version is 1:0:0,
		author is 'Lindsey Spratt',
		date is 2022-2-12,
		comment is 'DCTG expression translation.'
	]).

	:- protected(t_lp/6).
	:- mode(t_lp(+expression, -list, -term, -term, -term, -term), one).
	:- info(t_lp/6, [
		comment is 'Translate the left-hand side of a DCTG expression.',
		argnames is ['ExpressionHead', 'StL', 'S', 'SR', 'Sem', 'ClauseHead']
	]).

	:- protected(t_rp/6).
	:- mode(t_rp(+expression, -list, -list, -term, -term, -evaluable), one).
	:- info(t_rp/6, [
		comment is 'Translate the left-hand side of a DCTG expression.',
		argnames is ['ExpressionBody', 'St', 'StR', 'S', 'SR', 'Goal']
	]).
	
	:- uses(logtalk, [
		print_message(debug, dctg, Message) as dbg(Message)
	]).

	:- op(1001,xfy,'...').
	:- op(1150, xfx, '::=').
	:- op(1175, xfx, <:>).
	:- op(1150, xfx, ::-).
	:- op(650, yfx, ^^).
	:- op(1120, xfx, <<+).
	:- op(1110, xfx, +>>).
  
	t_lp((LP,List), StL, S, SR, Sem, H) :-
		!,
		{'$must_be_type'(list, List)},
		append(List, SR, List2),
		utilities::add_extra_args([node(LP, StL, Sem), S, List2], LP, H).
	t_lp(LP, StL, S, SR, Sem, H) :-
		utilities::add_extra_args([node(LP, StL, Sem), S, SR], LP, H).

	t_rp(!, St, St, S, S, !) :- !.
	t_rp([], St, [[]|St], S, S1, S=S1) :- !.
	t_rp([X], St, [[NX]|St], S, SR, ::c(S, X, SR)) :-
		dbg('    t_rp 3: ~w'+[rp([X], St)]),
		utilities::char(X, NX),
		!.
	t_rp([X], St, [X|St], S, SR, ::c(S, X, SR)) :- 
		dbg('    t_rp 4: ~w'+[rp([X], St)]),
		!.
	t_rp([X|R], St, [[NX|NR]|St], S, SR, (c(S, X, SR1), RB)) :-
		utilities::char(X, NX),
		!,
		{'$must_be_var_or_type'(list, R)},
		t_rp(R, St, [NR|St], SR1, SR, RB).
	t_rp([X|R], St, [[X|NR]|St], S, SR, (::c(S, X, SR1), RB)) :-
		!,
		{'$must_be_var_or_type'(list, R)},
		t_rp(R, St, [NR|St], SR1, SR, RB).
	t_rp({T}, St, St, S, S, T) :- 
		!,
		{'$must_be_type'(callable, T)}.
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
		utilities::add_extra_args([N, S, SR], T, Tt).
	t_rp(T, St, [St1|St], S, SR, Tt) :-
		utilities::add_extra_args([St1, S, SR], T, Tt).

:- end_category.
