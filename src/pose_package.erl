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
%% Copyright 2013 Beads D. Land-Trujillo.  All Rights Reserved.
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Parse transform for inserting `import/1' directives into packaged 
%% modules.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2013 Beads D. Land-Trujillo

%% @version 0.1.1
-module(pose_package).
-version("0.1.1").

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").
-include_lib("pose/include/macro.hrl").

%%
%% Exported Functions
%%

-export([parse_transform/2]).

%%
%% API Functions
%%

parse_transform(Tree, _Options) -> 
  insert_imports(parse_trans:get_file(Tree), Tree, []).
  
%%
%% Local Functions
%%

insert_imports(_File, [], List) -> lists:reverse(List);
insert_imports(File, [{attribute, _, module, [_]} = Head | Tail], List) -> 
  insert_imports(File, Tail, [Head | List]);
insert_imports(File, [{attribute, Line, module, _Module}=Head | Tail], List) ->
  Imports = pose_syntax:qualifiers(File),
  Inserts = [{attribute, Line, import, [X]} || X <- Imports],
  insert_imports(File, Tail, lists:append(Inserts, [Head | List]));
insert_imports(File, [Head | Tail], List) -> 
  insert_imports(File, Tail, [Head | List]).
  