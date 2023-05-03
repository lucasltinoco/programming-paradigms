{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Kojun (solveKojun) where

findEmptyLocationRecursive :: [[Int]] -> [Int] -> (Bool, [Int])
findEmptyLocationRecursive arr location@[row, col]
  | row >= length arr = (False, location)
  | col >= length arr = findEmptyLocationRecursive arr [row + 1, 0]
  | arr !! row !! col == 0 = (True, location)
  | otherwise = findEmptyLocationRecursive arr [row, col + 1]

-- Encontra a primeira célula vazia
findEmptyLocation :: [[Int]] -> [Int] -> [Int]
findEmptyLocation arr location = if found then foundLocation else [-1]
  where (found, foundLocation) = findEmptyLocationRecursive arr location

-- Verifica se o número já foi usado na mesma região
usedInSameRegionRecursive :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> Bool
usedInSameRegionRecursive arr row col num numRegion i j regionsArr
  | i >= length arr = False
  | j >= length arr = usedInSameRegionRecursive arr row col num numRegion (i + 1) 0 regionsArr
  | i == row && j == col = usedInSameRegionRecursive arr row col num numRegion i (j + 1) regionsArr
  | (arr !! i !! j) == num && (regionsArr !! i !! j) == numRegion = True
  | otherwise = usedInSameRegionRecursive arr row col num numRegion i (j + 1) regionsArr

-- Verifica se o número é maior que o tamanho da região
isBiggerThanRegionSize :: Int -> Int -> Bool
isBiggerThanRegionSize num regionCellsCount = regionCellsCount < num

-- Conta o número de células na região
countCellsInRegion :: [[Int]] -> Int -> Int -> Int -> Int -> Int
countCellsInRegion regionsArr numRegion row col count
  | row >= length regionsArr = count
  | col >= length regionsArr = countCellsInRegion regionsArr numRegion (row + 1) 0 count
  | regionsArr !! row !! col == numRegion = countCellsInRegion regionsArr numRegion row (col + 1) (count + 1)
  | otherwise = countCellsInRegion regionsArr numRegion row (col + 1) count

-- Verifica se o número já foi usado na mesma região ou se é maior que o tamanho da região
usedInSameRegionOrIsBiggerThanRegionSize :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Bool
usedInSameRegionOrIsBiggerThanRegionSize arr row col num regionsArr =
  let numRegion = regionsArr !! row !! col
      regionCellsCount = countCellsInRegion regionsArr numRegion 0 0 0
  in usedInSameRegionRecursive arr row col num numRegion 0 0 regionsArr || isBiggerThanRegionSize num regionCellsCount

-- Verifica se o número já foi usado em uma célula ortogonal
usedInOrthogonalCell :: [[Int]] -> Int -> Int -> Int -> Bool
usedInOrthogonalCell arr row col num =
  let topIsEqual = (row > 0) && (arr !! (row - 1) !! col == num)
      bottomIsEqual = (row < (length arr - 1)) && (arr !! (row + 1) !! col == num)
      leftIsEqual = (col > 0) && (arr !! row !! (col - 1) == num)
      rightIsEqual = (col < (length arr - 1)) && (arr !! row !! (col + 1) == num)
  in topIsEqual || bottomIsEqual || leftIsEqual || rightIsEqual

-- Verifica se a célula de cima é menor ou se a de baixo é maior
topCellInTheSameRegionIsLowerOrBottomIsBiggerIfExists :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Bool
topCellInTheSameRegionIsLowerOrBottomIsBiggerIfExists arr row col num regionsArr =
  let numRegion = regionsArr !! row !! col
      topNum = if row > 0 then Just (arr !! (row - 1) !! col) else Nothing
      topNumRegion = if row > 0 then Just (regionsArr !! (row - 1) !! col) else Nothing
      bottomNum = if row < length arr - 1 then Just (arr !! (row + 1) !! col) else Nothing
      bottomNumRegion = if row < length arr - 1 then Just (regionsArr !! (row + 1) !! col) else Nothing
  in case (topNumRegion, topNum, bottomNumRegion, bottomNum) of
      (Just r1, Just n1, _, _) | r1 == numRegion && n1 < num -> True
      (_, _, Just r2, Just n2) | r2 == numRegion && n2 > num -> True
      _ -> False

-- Verifica se a célula é segura para ser preenchida
checkLocationIsSafe :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Bool
checkLocationIsSafe arr row col num regionsArr =
  not (usedInSameRegionOrIsBiggerThanRegionSize arr row col num regionsArr) &&
  not (usedInOrthogonalCell arr row col num) &&
  not (topCellInTheSameRegionIsLowerOrBottomIsBiggerIfExists arr row col num regionsArr)

-- Gera uma matriz atualizada com o número na posição especificada
generateUpdatedMatrix :: [[Int]] -> Int -> Int -> Int -> [[Int]]
generateUpdatedMatrix matrix row col num =
  let newRowArr = map (\(item, colIndex) -> if colIndex == col then num else item) $ zip (matrix !! row) [0..]
  in map (\(rowArr, rowIndex) -> if rowIndex == row then newRowArr else rowArr) $ zip matrix [0..]

-- Preenche a célula com o número especificado
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

-- Resolve o desafio Kojun: função principal
solveKojun :: [[Int]] -> [[Int]] -> Maybe [[Int]]
solveKojun arr regionsArr =
  let emptyLocation = findEmptyLocation arr [0, 0]
  in
    if length emptyLocation == 2 then
      fillCellWithNumber arr (head emptyLocation) (emptyLocation !! 1) 1 regionsArr
    else
      Just arr