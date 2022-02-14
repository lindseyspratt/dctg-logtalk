# dctg-logtalk
# Definite Clause Translation Grammar

The major predicates are `dctg::process/2` and `dctg::consult/2`, which
consults a file of DCTG rules.

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

In Abramson's implementation, `dctg::process/2` is called `translate_rule/2`.
