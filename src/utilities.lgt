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


:- object(utilities).
	:- info([
		version is 1:0:0,
		author is 'Lindsey Spratt',
		date is 2022-2-14,
		comment is 'DCTG utilities.'
	]).

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

	dctg_inputDCTG(Input, _, Input, _).

	dctg_input_tailDCTG(Tail, _, _, Tail).

:- end_object.
