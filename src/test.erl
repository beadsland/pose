%% Author: Beads
%% Created: May 17, 2012
%% Description: TODO: Add description to test

-define(module, test).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([hello/0]).

%%
%% API Functions
%%

hello() ->
  io:format("~s~n", ["Hello, world!"]).

%%
%% Local Functions
%%

