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


:- category(dctg_tools).

	:- info([
		version is 1:0:0,
		author is 'Lindsey Spratt',
		date is 2022-02-14,
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
