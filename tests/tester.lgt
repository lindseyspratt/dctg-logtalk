:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(lgtunit(loader)),
	logtalk_load([
		'../src/utilities',
		'../src/dctg_print_tree',
		'../src/translate',
		'../src/dctg_tools',
		'../src/evaluate',
		'../src/dctg_process'
		], 
		[source_data(on), debug(on)]),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
