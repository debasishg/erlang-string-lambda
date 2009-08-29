%%
%% Ported from the String Lambdas in Oliver Steele's Functional Javascript
%% http://osteele.com/sources/javascript/functional/
%%
%% This work is licensed under the MIT License:
%%
%% (c) 2007 Debasish Ghosh
%% Portions Copyright (c) 2006 Oliver Steele
%% 
%% Permission is hereby granted, free of charge, to any person obtaining
%% a copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
%% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
%% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-module(lib_lambda_utils).

%%
%% Exported Functions
%%
-export([map/2, compose/1, reduce/3, foldl/3, foldr/3, select/2, filter/2, all/2, all_and/1]).

map(Fn, List) -> 
	lists:map(lib_lambda:lambda(Fn), List).

compose(Fns) ->
    lib_misc:compose(lists:map(fun lib_lambda:lambda/1, Fns)).

reduce(Fn, Init, List) ->
    lists:foldl(lib_lambda:lambda(Fn), Init, List).

foldl(Fn, Init, List) -> reduce(Fn, Init, List).

foldr(Fn, Init, List) -> 
    lists:foldr(lib_lambda:lambda(Fn), Init, List).

select(Pred, List) ->
    [Elem || Elem <- List, (lib_lambda:lambda(Pred))(Elem) == true].

filter(Pred, List) ->
    lists:filter(lib_lambda:lambda(Pred), List).
    
all(Pred, List) ->
    lists:all(lib_lambda:lambda(Pred), List). 

all_and(Preds) ->  
    lib_misc:all_and(lists:map(fun lib_lambda:lambda/1, Preds)).
