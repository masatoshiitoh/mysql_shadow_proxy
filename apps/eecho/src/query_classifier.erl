-module(query_classifier).

-export([is_modify_operator/1]).
-export([is_modify_operator/2]).
-export([find_operator/2]).

is_modify_operator(D) ->
    is_modify_operator(D, ["INSERT ", "UPDATE ", "DELETE "]).

is_modify_operator(_D, []) ->
    false;
is_modify_operator(D, Ops) ->
    [Ope | T] = Ops,
    case find_operator(D, Ope) of
        false -> is_modify_operator(D, T);
        _ELSE -> true
    end.

%%
%% 全
%%
find_operator(D, Ope) ->
    UpperOpe = list_to_binary(string:uppercase(Ope)),
    LowerOpe = list_to_binary(string:lowercase(Ope)),
    case binary:match(D, UpperOpe) of
        nomatch ->
            %% upperope に合致しなかったら、 loweropeと比較し、nomatchじゃなければtrue
            binary:match(D, LowerOpe) /= nomatch;
        _ELSE ->
            %% upperopeに合致したら true
            true
    end.

