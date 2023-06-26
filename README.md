# Paradigmas de Programação

O presente repositório tem por objetivo guardar o desenvolvimento dos projetos
desenvolvidos na disciplina de Paradigmas de Programação da Universidade Federal
de Santa Catarina no semestre 2023-1.

- Nome: Lucas de Lacerda Tinoco
- Professor: Maicon Rafael Zatelli

## Projeto 1: Solução do Kojun em Haskell

O primeiro projeto teve por objetivo solucionar o jogo
[Kojun](https://www.janko.at/Raetsel/Kojun/index.htm) em Haskell, com intuito de
exercitar o paradigma funcional. Kojun, é um jogo parecido com o Sudoku, porém
com divisões e regras diferentes. Como, a solução do Sudoku é um pouco mais
trivial e pode ser feita utilizando a mesma técnica de backtracking, optou-se
por, primeiramente, desenvolver um algoritmo para solucionar tal jogo em
Javascript (`javascript/src/sudoku.js`).

Após ter solucionado o Sudoku em Javascript, foi concluída a implementação do
algoritmo que soluciona o Kojun: primeiramente em Javascript de forma iterativa,
depois em Javascript de forma recursiva e imutável e depois em Haskell.

### Explicação do código em Haskell

A função principal `solveKojun` recebe uma matriz com o input do desafio e uma
matriz com as regiões do desafio, devolvendo o resultado final, se houver. Ela
checa inicialmente se a matriz de entrada tem posições vazias e, se sim, retorna
o esta matriz, senão, prossegue para chamada da função `fillCellWithNumber`:

```haskell
solveKojun :: [[Int]] -> [[Int]] -> Maybe [[Int]]
solveKojun arr regionsArr =
  let emptyLocation = findEmptyLocation arr [0, 0]
  in
    if length emptyLocation == 2 then
      fillCellWithNumber arr (head emptyLocation) (emptyLocation !! 1) 1 regionsArr
    else
      Just arr
```

A função `fillCellWithNumber` recebe a matrz de entrada, a posição da linha e
coluna do número atual, o número atual, a matriz de regiões e retorna uma matriz
ou nada (`Nothing`), que é o caso em que não há solução para o exercício (número
atual é maior que o tamanho da matriz).

Caso a localização da linha e coluna seja apropriada para o número da vez
(`checkLocationIsSafe` retorna `True`), prossegue-se a execução do programa
reexecutando `solveKojun` agora com a matriz atualizada. Se, porventura, essa
nova chamada de `solveKojun` devolve `Nothing`, repete-se a chamada de
`fillCellWithNumber`, com a posição da matriz resetada para 0 e incrementando-se
o número, para que ocorra uma nova tentativa.

Se nenhum dos casos acima ocorre, repete-se a chamada de `fillCellWithNumber`,
com a posição da matriz resetada para 0 e incrementando-se o número, para uma
nova tentativa.

```haskell
fillCellWithNumber :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Maybe [[Int]]
fillCellWithNumber arr row col num regionsArr
  | num > length arr = Nothing
  | checkLocationIsSafe arr row col num regionsArr =
      let resultArray = solveKojun (generateUpdatedMatrix arr row col num) regionsArr
      in
        if resultArray /= Nothing then
          resultArray
        else
          fillCellWithNumber (generateUpdatedMatrix arr row col 0) row col (num + 1) regionsArr
  | otherwise =
      fillCellWithNumber (generateUpdatedMatrix arr row col 0) row col (num + 1) regionsArr
```

`checkLocationIsSafe` é uma função que recebe a matriz, posição, o número e
matriz de regiões, retornando se a posição é aproriada para o número:

```haskell
checkLocationIsSafe :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Bool
checkLocationIsSafe arr row col num regionsArr =
  not (usedInSameRegionOrIsBiggerThanRegionSize arr row col num regionsArr) &&
  not (usedInOrthogonalCell arr row col num) &&
  not (topCellInTheSameRegionIsLowerOrBottomIsBiggerIfExists arr row col num regionsArr)
```

As funções internas correspondem às contrarregras do jogo Kojun. Se alguma delas
devolve `True`, o elemento não pode ser posicionado na linha e coluna
especificadas:

- `usedInSameRegionOrIsBiggerThanRegionSize`: checa se o número é utilizado na
  mesma região ou se é maior que o tamanho da região em que está inserido;
- `usedInOrthogonalCell`: checa se é utilizado na célula da esquerda, direita,
  de cima ou de baixo;
- `topCellInTheSameRegionIsLowerOrBottomIsBiggerIfExists`: checa se célula
  superior de mesma região é menor ou se célula inferior na mesma região é
  menor, se existente.

### Como testar?

Para testar em Haskell, basta executar `cabal run` no diretório `/haskell`. O
arquivo `Main.hs`, que testa a solução para os casos de matriz 6x6, 10x10,
12x12, 14x14 e 17x17, será executado devolvendo o resultado das operações no
terminal. Para testar em Javascript, basta rodar `yarn test` no diretório
`javascript/`.

### Dificuldades encontradas

Inicialmente, foi difícil implementar o código em Haskell após o ter solucionado
em Javascript, pois a solução havia sido implementada de forma iterativa. Após
essa dificuldade, o código em Javascript foi convertido para forma recursiva e
imutável. Após essa refatoração ficou mais tranquilo de fazer a conversão,
contudo foram encontrados obstáculos no debugging do código, pois a solução não
funcionou imediatamente em Haskell. Para solucionar isto, as funções foram
analisadas e testadas na main, uma a uma, chegando ao resultado final.

Intencionava-se, também, desenvolver uma suíte de testes com a biblioteca HUnit,
similar ao que foi feito em Javascript, mas não foi possível configurá-la.

## Projeto 2: Solução do Kojun em Clojure

O segundo projeto também teve por objetivo solucionar o jogo Kojun, porém agora
em Clojure, com o intuito de exercitar o paradigma funcional em outra linguagem.
O código é muito parecido com o de Haskell, porém com algumas diferenças, visto
que Clojure é uma linguagem baseada em Lisp. Aqui está um exemplo de código da
função fill-dell-with-number em Clojure:

```clojure
(defn fill-cell-with-number [arr row col num regions-arr]
  (if (> num (count arr))
    nil
    (let [location-is-safe (check-location-is-safe arr row col num regions-arr)]
      (if location-is-safe
        (let [result-array
              (solve-kojun (generate-updated-matrix arr row col num) regions-arr)]
          (if result-array
            result-array
            (fill-cell-with-number (generate-updated-matrix arr row col 0) row col (inc num) regions-arr)))
        (fill-cell-with-number (generate-updated-matrix arr row col 0) row col (inc num) regions-arr)))))
```

### Como testar?

Para testar em Clojure, basta executar `lein test` no diretório `/clojurep`. O
programa irá executar os testes do arquivo `core_test.clj` para desafios em 6x6,
8x8, 10x10, 12x12, 14x14 e 17x17, mostrando o resultado no terminal. Também é
possível executar o programa principal para configurar chamada da função
`solve-kojun` com os parâmetros desejados, basta executar `lein run` no
diretório `/clojurep`.

### Dificuldades encontradas

Notoriamente, o desafio desta vez não foi implementar a lógica do algoritmo,
visto que já havia sido feito em Haskell e Javascript. A dificuldade foi em
configurar o ambiente de desenvolvimento, visto que Clojure é uma linguagem que
não é tão popular quanto as outras e, portanto, não há muita documentação sobre.
Após muita pesquisa, foi possível configurar o ambiente de desenvolvimento e
implementar o algoritmo. Além disso, a adaptação a sintaxe baseada em Lisp e com
muitos parênteses foi um pouco difícil, mas nada que não tenha sido superado. Em
Clojure, porém, foi possível desenvolver uma suíte de testes assim como em
Javascript.

## Projeto 3: Solução do Kojun em Prolog

O terceiro projeto teve por objetivo solucionar o jogo Kojun em Prolog, com o
intuito de exercitar o paradigma lógico. O código é muito parecido com o de
Haskell e Clojure, porém com algumas diferenças, visto que Prolog é uma
linguagem baseada em lógica. A execução em Prolog foi muito mais rápida que nas
outras linguagens.

Foi criado um arquivo `kojun.pl` com as funções envolvidas na solução do Kojun e
um outro arquivo `test_kojun.pl` com os testes para matrizes 6x6, 8x8, 10x10,
12x12, 14x14 e 17x17. Aqui está a função principal do código em Prolog:

```prolog
% Função principal para resolver o kojun.
kojun(Lines) :-
    maplist(limits(Lines), Lines),
    check_repetition_rule(Lines),
    maplist(check_adjacency, Lines),
    transpose(Lines, Columns),
    maplist(check_adjacency, Columns),
    maplist(check_vertical_rule, Columns).
```

### Como testar?

Para testar em Prolog, basta executar `swipl test_kojun.pl` no diretório
`/prolog`. O programa irá executar os testes do arquivo `test_kojun.pl` para os
tamanhos de matrizes citados acima.

### Dificuldades encontradas

A maior dificuldade foi em entender a sintaxe da linguagem, visto que é muito
diferente das outras linguagens que já havia trabalhado. Após entender a
sintaxe, foi possível implementar o algoritmo sem muitas dificuldades. Além
disso, foi possível desenvolver uma suíte de testes assim como em Javascript e
Clojure.
