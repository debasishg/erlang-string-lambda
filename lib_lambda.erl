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


-module(lib_lambda).
-author('dghosh@acm.org').
-copyright('Copyright (c)2007 Debasish Ghosh. All rights reserved.').

%%
%% Exported Functions
%%
-export([lambda/1]).

-define(UNARY_ARG, "_").
-define(UNARY_ARG_MATCH, "(^_|[\s()-*+<>\/',]+_|[-]_)").
-define(UNARY_PARAM, "X").
-define(LEFT_MATCH_PARAM, ?UNARY_PARAM).
-define(RIGHT_MATCH_PARAM, "Y").
-define(NULL_STRING, "").
-define(EMPTY_STRING, " ").
-define(FUN_TEMPLATE, "~sfun(~s)->~s end~s~s").

%% @spec lambda(string()) -> function()
%% @doc Implements string lambdas based on Oliver Steele's implementation in Javascript.

lambda(String) ->
    case regexp:split(String, "\s*->\s*") of
        
        %% If the string contains a `->`, this separates the parameters from the body:
        %% >> lambda('X Y -> X+Y')(23, 12) => 35
        %% >> lambda('X, Y -> 2*X+Y')(23, 12) => 58
        %% >> lambda('X -> X+1')(1) => 2
        %% >> lambda('X -> Y -> 2*X+Y')(1, 4) => 6
        
        {ok, [H|T]} when length([H|T]) > 1 -> 
            form_lambda_and_eval(lists:reverse([H|T]));
        
        %% Otherwise, if the string contains a `_`, this is the parameter:                                 
        %% >> lambda('_ + 12')(3) => 15

        {ok, [H|_]} ->
            case match_unary_arg_and_sub(H) of
                {ok, NewString, RepCount} when RepCount > 0 ->
                    Params = ?UNARY_PARAM,
                    FunString = lib_misc:join(["fun(", Params, ") ->", NewString, " end."]), 
                    eval(FunString);
                                                          
                %% test whether an operator appears on the left (or right), respectively
                %% >> lambda('2/')(4) => 0.5
                %% >> lambda('/2')(6) => 3
                %% >> lambda('/')(6,2) => 3
                %% >> lambda('4/2')(78) => 2 - ignore the argument
                                                              
                {ok, _, 0} -> 
                    {LParams, LBody} = left_section_match(H),
                    {RParams, RBody} = right_section_match(H, LParams, LBody),

                    %% take care of false positives
                    %% >> lambda('4/2') => fun(X_) -> 4/2 end
                    case (length(RParams) == 0) and (length(RBody) == 0) of
                        true ->
                            %% need to check if we can capture any Erlang variable
                            %% if not, then make the function return the expression
                            %% with a single argument, so that it can be applied with
                            %% higher order functions
                                
                            case capture_variables(H) of
                                {ok, Variables} when length(Variables) > 0 ->
                                    FunString = lib_misc:join(["fun(", lib_misc:join(Variables, ","), ")->", H, " end."]),
                                    eval(FunString);
                                {ok, _} -> 
                                    FunString = lib_misc:join(["fun(", ?UNARY_PARAM, ")->", H, " end."]),
                                    eval(FunString);
                                {error, Error} -> {error, Error}
                            end;
                                
                        _ ->
                            FunString = lib_misc:join(["fun(", RParams, ")->", RBody, " end."]),
                            eval(FunString)
                    end
            end;
        {error, Error} -> {error, Error}
    end.

%% Local Functions
%%

%% for lambdas containing ->, form the lambda body and eval
%% lambda('X -> Y -> 2*X + Y') => (fun(X) -> (fun(Y) -> 2*X + Y end) end).

form_lambda_and_eval([H|T]) ->
    Expr = H,
    Length = length([H|T]),
    case length([H|T]) of
        1 -> 
            %% since _ is allowed in string lambda
            %% replace underscore with a valid erlang variable name
            %% >> lambda('_->_+4')(8) => fun(X_) -> X_ + 4 end => 12
                
            {ok, NewString, _} = match_unary_arg_and_sub(H),
            eval(string:concat(NewString, "."));
        _ ->
            case regexp:split(hd(T), "\s*,\s*|\s+") of
                {ok, _} when Length == 1 -> eval(H);
                {ok, Lhs} ->
                    Params = lib_misc:join(Lhs, ","),
                    Body = lib_misc:join(["(fun(", Params, ")->", Expr, " end)"]),
                    TRest = T -- [hd(T)],
                    form_lambda_and_eval([Body] ++ TRest);
                {error, Error} -> {error, Error}
            end
     end.
    
%% left section match
%% >> lambda(' /3')(27) => 9
%% >> lambda('/4')(12) => 3

left_section_match(String) ->
    case regexp:first_match(String, "^\s*([+*\/(div)(rem)(bnot)(band)(bor)(bxor)(bsl)(bsr)|=<>])") of
        {match,_,_} ->
            Params = ?LEFT_MATCH_PARAM,
            Body = lib_misc:join([Params, String], ?EMPTY_STRING),
            {Params, Body};
        _ -> {?NULL_STRING, ?NULL_STRING}
    end.

%% right section match
%% >> lambda(' 9/')(3) => 3
%% >> lambda('/')(12,6) => 2

right_section_match(String, LParams, LBody) ->
    case regexp:first_match(String, "[+*\-\/(div)(rem)(band)(bor)(bxor)(bsl)(bsr)=<>]\s*$") of
        {match,_,_} ->
            Params = ?RIGHT_MATCH_PARAM,
            case length(LBody) of
                0 -> 
                    {Params, lib_misc:join([String, Params], ?EMPTY_STRING)};
                _ -> 
                    {lib_misc:join([LParams, Params], ","), string:concat(LBody, Params)}
            end;
        _ -> {LParams, LBody}
    end.

%%

%% get the list of variables according to the order in which
%% they appear in the string lambda
%% >> 'X+2*Y' => [X,Y]
%% >> 'X+2*Y+X' => [X,Y]
%% >> 'Y+2*X' => [Y,X]

capture_variables(String) ->
    case regexp:matches(String, "[\s\b\f\n]*[A-Z][0-9a-zA-Z]*[\s\b\f\n]*") of
        {match, Matches} ->
            {ok, lists:foldl(
                fun insert_if_absent/2,
                [], [string:strip(string:substr(String, S, L), both) || {S,L} <- Matches])};
        {error, Error} -> {error, Error}
    end.

insert_if_absent(String, List) ->
    case lists:member(String, List) of
        false -> List ++ [String];
        true -> List
    end.

eval(S) ->
    {ok,Scanned,_} = erl_scan:string(S),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    {value, Value,_} = erl_eval:exprs(Parsed,[]),
    Value.

%% match unary arg (_) and replace with X
%% "_->_+1" => {ok, "X->X+1", 2}
%% could not use the built-in regexp:gsub/3, since it does not
%% support word boundary matching as perl (\b)

match_unary_arg_and_sub(String) ->
    case regexp:matches(String, ?UNARY_ARG_MATCH) of
        {match, Matches} ->
            Positions = [S+L-1 || {S,L} <- Matches],
            replace(String, Positions, 0);
        {error, Error} -> {error, Error}
    end.

replace(String, [], RepCount) -> {ok, String, RepCount};
replace(String, [First|Rest], RepCount) ->
    case First of
        1 -> Str = ?UNARY_PARAM;
        _ -> Str = string:concat(string:substr(String, 1, First - 1), ?UNARY_PARAM)
    end,
    replace(string:concat(Str, string:substr(String, First + 1, length(String) - length(Str))), Rest, RepCount + 1).

   
