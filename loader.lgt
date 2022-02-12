
:- initialization((
	logtalk_load(basic_types(loader)),
	logtalk_load(os(loader)),
	logtalk_load([
		'src/utilities',
		'src/dctg_print_tree',
		'src/translate',
		'src/dctg_process'
	], [optimize(on)])
)).
