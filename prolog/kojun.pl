:- use_module(library(clpfd)).

% Verifica a adjacência entre as regiões em uma linha.
check_adjacency([]).
check_adjacency([_|[]]).
check_adjacency([_-Left, Region-Right|Tail]) :-
    Left #\= Right,
    check_adjacency([Region-Right|Tail]).

% Verifica a regra vertical entre duas regiões.
check_vertical_rule([]).
check_vertical_rule([_|[]]).
check_vertical_rule([UpperRegion-UpperValue, LowerRegion-LowerValue|Tail]) :-
    (UpperRegion #\= LowerRegion) #\/ (UpperValue #> LowerValue),
    check_vertical_rule([LowerRegion-LowerValue|Tail]).

% Obtém o maximo valor em uma região.
max_region(Tab, Region, Max) :-
    append(Tab, UniqueRow),
    sort(UniqueRow, SortedRow),
    group_pairs_by_key(SortedRow, Groups),
    member(Region-Value, Groups),
    length(Value, Max).

% Define os limites de valores para as regiões no tabuleiro.
limits(Tab, [Region-Value|[]]) :-
    max_region(Tab, Region, Max),
    Value in 1..Max.
limits(Tab, [Region-Value, Next|Tail]) :-
    max_region(Tab, Region, Max),
    Value in 1..Max,
    limits(Tab, [Next|Tail]).

% Verifica se os valores em uma região são distintos.
distinct_values(_-List) :- all_distinct(List).

% Verifica a regra de repetição de valores no tabuleiro.
check_repetition_rule(Tab) :-
    append(Tab, UniqueRow),
    sort(UniqueRow, SortedRow),
    group_pairs_by_key(SortedRow, Groups),
    maplist(distinct_values, Groups).

% Obtém os valores de uma lista de pares (chave-valor).
get_values([], []).
get_values([H|T], [H2|T2]) :-
    pairs_values(H, H2),
    get_values(T, T2).

% Atribui valores a cada variavel da lista.
guess(Lines) :-
    get_values(Lines, Values),
    maplist(label, Values).

% Função principal para resolver o kojun.
kojun(Lines) :-
    maplist(limits(Lines), Lines),
    check_repetition_rule(Lines),
    maplist(check_adjacency, Lines),
    transpose(Lines, Columns),
    maplist(check_adjacency, Columns),
    maplist(check_vertical_rule, Columns).
