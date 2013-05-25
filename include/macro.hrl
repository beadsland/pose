%% CDDL HEADER START    -*-Erlang-*-
%% -----------------------------------------------------------------------
%% The contents of this file are subject to the Common Development and
%% Distribution License, Version 1.0 (the "License"); you may not use
%% this file except in compliance with the License.  You should have
%% received a copy of the Common Development and Distribution License
%% along with this software.  If not, it can be retrieved online at
%% http://www.opensource.org/licenses/CDDL-1.0
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% When distributing Covered Code, include this CDDL Header Notice in
%% each file and include the License file at CDDL-LICENSE.  If applicable
%% add the following below the CDDL Header, with the fields enclosed
%% by brackets replaced by your own identifying information:
%% "Portions Copyright [year] [name of copyright owner]"
%%
%% Copyright 2012, 2013 Beads D. Land-Trujillo.  All Rights Reserved.
%% ?FUNCTION/0 Copyright 2007 G. Bulmer.
%% -----------------------------------------------------------------------
%% CDDL HEADER END

-ifdef(package).
-import(proplists).
-endif.
-define(ATTRIB(Module, Attribute),
    proplists:get_value(Attribute, Module:module_info(attributes))).
-define(VERSION(Module), gen_command:get_version(Module)).


% Original source: 
%   http://erlang.2086793.n4.nabble.com/Why-no-FUNCTION-macro-td2099004.html
% Hacked to run w/o variables.
-define(CURRFUNC, process_info(self(), current_function)).
-define(FUNCTION,
        case ?CURRFUNC of {_, {_,_,_}} -> element(2,element(2,?CURRFUNC)) end).
-define(ARITY,
        case ?CURRFUNC of {_, {_,_,_}} -> element(3,element(2,?CURRFUNC)) end).

-ifdef(package).
-import(lists).
-import(io_lib).
-endif.
-define(WHEREAMI,
       lists:flatten(io_lib:format("~s:~p/~p, line ~p",
                                   [atom_to_list(?MODULE),
                                   ?FUNCTION, ?ARITY, ?LINE]))).

-define(BAIL(X), exit({?WHEREAMI, X})).