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
em Javascript, pois a solução havia sido implementada de forma iterativa. Após essa
dificuldade, o código em Javascript foi convertido para forma recursiva e
imutável. Após essa refatoração ficou mais tranquilo de fazer a conversão,
contudo foram encontrados obstáculos no debugging do código, pois a solução não
funcionou imediatamente em Haskell. Para solucionar isto, as funções foram
analisadas e testadas na main, uma a uma, chegando ao resultado final.

Intencionava-se, também, desenvolver uma suíte de testes com a biblioteca HUnit,
similar ao que foi feito em Javascript, mas não foi possível configurá-la.
