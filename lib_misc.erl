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

-module(lib_misc).

%%
%% Exported Functions
%%
-export([split_n_each/2, split_into_n/2, join/2, compose/1, all_and/1, join/1]).

split_n_each(N, List) when length(List) < N -> [List];

split_n_each(N, List) ->
    split_n_each(N, List, []).

split_into_n(N, List) when length(List) < N -> split_n_each(1, List);
split_into_n(N, List) -> split_n_each(length(List) div N, List).

%% @spec compose([function()]) -> function()
%% @doc Composing by chaining backward.

compose([Fn]) -> Fn;
compose([Fn | FnRest]) ->
  fun(Args) ->
    apply(Fn, [apply(compose(FnRest), [Args])])
  end.

%% returns the boolean result after applying all predicates
%% to all the arguments.

all_and([]) -> true;
all_and([Pred]) ->
    fun(Args) ->
        apply(Pred, [Args])
    end;
all_and([Pred | PredTail]) ->
    fun(Args) ->
        case apply(Pred, [Args]) of
            true -> apply(all_and(PredTail), [Args]);
            false -> false
        end
    end.

%% join a list of strings to form a single string

join(L) -> joinX(L, []).
joinX([X], Acc) -> Acc ++ X;
joinX([X | T], Acc) -> joinX(T, Acc ++ X).                                                      
    
%%
%% Local Functions
%%

split_n_each(_, [], Acc) -> Acc;
split_n_each(N, List, Acc) when length(List) < N 
                          -> split_n_each(N, [], Acc ++ [List]);
split_n_each(N, List, Acc) ->
    {List1, List2} = lists:split(N, List),
    split_n_each(N, List2, Acc ++ [List1]).

%% join a list of strings to form a single string
%% each separated by a separator

join(List, Separator) ->
    string:substr(joinx(fun thing_to_list/1, List, Separator), 2).

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_list(X)    -> X.	%Assumed to be a string

joinx(F, [Hd|Tail], Separator) ->
    Separator ++ F(Hd) ++ joinx(F, Tail, Separator);
joinx(F, [], _) when is_function(F, 1) -> [].









