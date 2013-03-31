Fun! Fun!
---------

This is a fun project that I did quite some time back. It's an adaptation of Oliver Steele's Functional Javascript
in Erlang. It gives us a nice DSL in Erlang that enables you to write lambdas in String form.

The following blog post summarizes the project .. http://debasishg.blogspot.com/2007/11/erlang-string-lambdas.html

Typically in Erlang you write ..

(fun(X) -> X * 2 end)(6)

to get 12 as the result. With string lambda, you write ..

(lib_lambda:lambda("X * 2"))(6)

to get the same result. Ok, counting the characters of the module and the function, you grin at the fact that it is really more verbose than the original one. But we have less boilerplates, with the explicit function declaration being engineered within the lambda.

A couple of more examples of using the string lambda :

(lib_lambda:lambda("X+2*Y+5*X"))(2,5) => 22
(lib_lambda:lambda("Y+2*X+5*Y"))(5,2) => 34


Less noisy ? Let's proceed ..

You can have higher order functions using the string lambdas, and with lesser noise .. The following are based on original Javascript examples in Oliver Steele's page ..

lists:map(fun(X) -> X * X end, [1,2,3]).

-> [1,4,9]

lists:filter(fun(X) -> X > 2 end, [1,2,3,4]).

-> [3,4]


lists:any(fun(X) -> length(X) < 3 end, string:tokens("are there any short words?", " ")).


-> false

and with string lambdas, you have the same result with ..

lib_lambda_utils:map("X*X", [1,2,3]).
lib_lambda_utils:filter("X>2", [1,2,3,4]).
lib_lambda_utils:any("length(_) < 3", string:tokens("are there any short words?", " ")).


In the last example, _ is the parameter to a unary function and provides a very handy notation in the lambda. 
Here are some more examples with unary parameter ..

%% multiply by 5
(lib_lambda:lambda("_*5"))(8).

-> 40


%% length of the input list > 2 and every element of the list > 3
(lib_lambda_utils:all_and(["length(_) > 2", "lib_lambda_utils:all(\">3\", _)"]))([1,2,3,4]).


-> false

Explicit Parameters
-------------------

As per original convention, -> separates the body of the lambda from the parameters, when stated explicitly and the 
parameters are matched based on their first occurence in the string lambda ..

(lib_lambda:lambda("X Y->X+2*Y"))(1,4).

-> 9

(lib_lambda:lambda("Y X->X+2*Y"))(1,4).

-> 6

Implicit Parameters
-------------------

If not specified explicitly, parameters can be implicit and can be effectively deduced ..

%% left section implicit match
(lib_lambda:lambda("/2"))(6)

-> 3

%% right section implicit match
(lib_lambda:lambda("10/"))(2)

-> 5

%% both sections implicit match
(lib_lambda:lambda("/"))(12,6)

-> 2

Chaining for Curry
------------------

The operator -> can be chained to implement curried functions.

((lib_lambda:lambda("X->Y->X+2*Y"))(1))(4).

-> 9

or deferred invocation ..

Fun=(lib_lambda:lambda("X->Y->X+2*Y"))(1).

-> #Fun<erl_eval.6.72228031>

Fun(4).

-> 9

Higher Order Functions
----------------------

String lambdas allow creating higher order functions with much less noise and much more impact than native 
boilerplates in the language.


%% compose allows composition of sequences of functions backwards
(lib_lambda_utils:compose("+1", "*2"))(1).


-> 3

%% higher order functions
lib_lambda_utils:map("+1", lib_lambda_utils:map("*2", [1,2,3])).

-> [3,5,7]

lists:map(lib_lambda:compose(["+1", "*2"]), [1,2,3]).

-> [3,5,7]

And here are some catamorphisms ..

lists:reverse() is typically defined by Erlang in the usual recursive style :

reverse([] = L) ->
    L;
reverse([_] = L) ->
    L;
reverse([A, B]) ->
    [B, A];
reverse([A, B | L]) ->
    lists:reverse(L, [B, A]).


Here is reverse() using catamorphism and delayed invocation through string lambdas ..

Reverse = lib_lambda:lambda("lib_lambda:foldl(\"[E] ++ S\", [], _)").

-> #Fun<erl_eval.6.72228031>

and later ..

Reverse([1,2,3,4]).

-> [4,3,2,1]

Or the classic factorial ..


Factorial = lib_lambda:lambda("lib_lambda:foldr(\"E*S\", 1, lists:seq(1,_))").


-> #Fun<erl_eval.6.72228031>

and later ..

Factorial(5).

-> 120

License
-------

This software is licensed under the Apache 2 license, quoted below.

Licensed under the Apache License, Version 2.0 (the "License"); you may not
use this file except in compliance with the License. You may obtain a copy of
the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
License for the specific language governing permissions and limitations under
the License.

