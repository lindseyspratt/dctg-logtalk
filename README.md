# dctg-logtalk
# Definite Clause Translation Grammar (DCTG)


Definite Clause Translation Grammar rules (DCTGs) are represented as:

	LeftPart ::= RightPart <:> Attributes ::- Semantics.

In this term, `LeftPart ::= RightPart` represents the rule syntax while
`Attributes ::- Semantics` represents the rule semantics. As in DCGs,
`LeftPart` is a non-terminal possibly followed by a list of terminals
(aka a "push-back" list).


## Usage

To load all the files, load the `loader.lgt` file in the main directory.

DCTG files must have a `.dctg` extension and must define the `dctg_main/2`
predicate where the first argument is the predicate indicator of the main
DCTG predicate and the second argument is the predicate indicator of the
evaluation predicate.

The major predicates are `dctg::process/2`, which translates a DCTG rule
to a clause, and `dctg::consult/2`, which consults a file with DCTG rules.


## Debugging

TBD.


## History

This Logtalk implementation of the Definite Clause Translation Grammar is
based on an implementation created by Lindsey Spratt in December, 1986,
which in turn was based on the "Definite Clause Translation Grammars" paper
by Harvey Abramson:

	@inproceedings{SLP-1984-Abramson84,
		author    = "Harvey Abramson",
		booktitle = "{Proceedings of the First International Symposium on Logic Programming}",
		isbn      = "0-8186-0522-7",
		pages     = "233--240",
		publisher = "{IEEE-CS}",
		title     = "{Definite Clause Translation Grammars}",
		year      = 1984,
	}

In Abramson's implementation, the `dctg::term_expansion/2` predicate is
called `translate_rule/2`.
