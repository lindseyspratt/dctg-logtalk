:- category(dctg_tools).

	:- info([
		version is 1:0:0,
		author is 'Lindsey Spratt',
		date is 2022-2-12,
		comment is 'Tools for running grammar produced by DCTG.'
	]).

	:- public(c/3).
	:- mode(c(+list, -list, -list), one).
	:- info(c/3, [
		comment is 'Head of first argument list is the second argument and rest of first argument list is the third argument.',
		argnames is ['List', 'Head', 'Tail']
	]).

	c([X|S], X, S).
:- end_category.