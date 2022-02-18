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


:- object(embedded).

	dctg_main(token/0, value/1).

	token ::=
		token_ls ^^ T
		<:> value(V) ::- T ^^ value(V).

	token_ls ::=
		tokenc ^^ C,
		token_ls ^^ Cs
		<:> value([H|T]) ::-
			C ^^ value(H),
			Cs ^^ value(T).
	token_ls ::=
		[]
		<:> value([]).

	tokenc ::=
		[X],
		{token_char(X)}
		<:> value(X).

	token_char(_).

:- end_object.
