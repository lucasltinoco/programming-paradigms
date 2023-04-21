module Main (main) where

findEmptyLocationRecursive :: [[Int]] -> [Int] -> Bool
findEmptyLocationRecursive arr location@[row, col]
  | row >= length arr = False
  | col >= length arr = findEmptyLocationRecursive arr [row + 1, 0]
  | arr !! row !! col == 0 = True
  | otherwise = findEmptyLocationRecursive arr [row, col + 1]

findEmptyLocation :: [[Int]] -> [Int] -> [Int]
findEmptyLocation arr location = if found then location else [-1]
  where found = findEmptyLocationRecursive arr location

countCellsInRegion :: [[Int]] -> Int -> Int -> Int -> Int -> Int
countCellsInRegion regionsArr numRegion row col count
  | row >= length regionsArr = count
  | col >= length regionsArr = countCellsInRegion regionsArr numRegion (row + 1) 0 count
  | regionsArr !! row !! col == numRegion = countCellsInRegion regionsArr numRegion row (col + 1) (count + 1)
  | otherwise = countCellsInRegion regionsArr numRegion row (col + 1) count

usedInSameRegionRecursive :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> Bool
usedInSameRegionRecursive arr row col num numRegion i j regionsArr
  | i >= length arr = False
  | j >= length arr = usedInSameRegionRecursive arr row col num numRegion (i + 1) 0 regionsArr
  | i == row && j == col = usedInSameRegionRecursive arr row col num numRegion i (j + 1) regionsArr
  | arr !! i !! j == num && regionsArr !! i !! j == numRegion = True
  | otherwise = usedInSameRegionRecursive arr row col num numRegion i (j + 1) regionsArr

isBiggerThanRegionSize :: Int -> Int -> Bool
isBiggerThanRegionSize num regionCellsCount = regionCellsCount < num

usedInSameRegionOrIsBiggerThanRegionSize :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Bool
usedInSameRegionOrIsBiggerThanRegionSize arr row col num regionsArr =
  let numRegion = regionsArr !! row !! col
      regionCellsCount = countCellsInRegion regionsArr numRegion row col 0
  in (usedInSameRegionRecursive arr row col num numRegion 0 0 regionsArr) || (isBiggerThanRegionSize num regionCellsCount)

usedInOrthogonalCell :: [[Int]] -> Int -> Int -> Int -> Bool
usedInOrthogonalCell arr row col num =
  let topIsEqual = row > 0 && (arr !! (row - 1)) !! col == num
      bottomIsEqual = row < length arr - 1 && (arr !! (row + 1)) !! col == num
      leftIsEqual = col > 0 && (arr !! row) !! (col - 1) == num
      rightIsEqual = col < length (arr !! row) - 1 && (arr !! row) !! (col + 1) == num
  in topIsEqual || bottomIsEqual || leftIsEqual || rightIsEqual

topCellInTheSameRegionIsLowerOrBottomIsBiggerIfExists :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Bool
topCellInTheSameRegionIsLowerOrBottomIsBiggerIfExists arr row col num regionsArr
  | row > 0 = let topNum = arr !! (row-1) !! col
                  topNumRegion = regionsArr !! (row-1) !! col
              in topNumRegion == numRegion && topNum < num
  | row < length arr - 1 = let bottomNum = arr !! (row+1) !! col
                               bottomNumRegion = regionsArr !! (row+1) !! col
                           in bottomNumRegion == numRegion && bottomNum > num
  | otherwise = False
  where numRegion = regionsArr !! row !! col

checkLocationIsSafe :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Bool
checkLocationIsSafe arr row col num regionsArr =
  not (usedInSameRegionOrIsBiggerThanRegionSize arr row col num regionsArr) &&
  not (usedInOrthogonalCell arr row col num) &&
  not (topCellInTheSameRegionIsLowerOrBottomIsBiggerIfExists arr row col num regionsArr)

generateUpdatedMatrix :: [[Int]] -> Int -> Int -> Int -> [[Int]]
generateUpdatedMatrix matrix row col num =
  let newRowArr = map (\(item, colIndex) -> if colIndex == col then num else item) $ zip (matrix !! row) [0..]
  in map (\(rowArr, rowIndex) -> if rowIndex == row then newRowArr else rowArr) $ zip matrix [0..]

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

solveKojun :: [[Int]] -> [[Int]] -> Maybe [[Int]]
solveKojun arr regionsArr =
  let emptyLocation = findEmptyLocation arr [0, 0]
  in
    if length emptyLocation == 2 then
      fillCellWithNumber arr (emptyLocation !! 0) (emptyLocation !! 1) 1 regionsArr
    else
      Just arr

inputArr :: [[Int]]
inputArr = [
    [5, 0, 2, 0, 2, 0, 3, 1, 3, 1],
    [0, 4, 0, 1, 0, 5, 0, 5, 0, 4],
    [7, 5, 1, 7, 0, 0, 3, 1, 3, 0],
    [0, 4, 0, 0, 0, 0, 0, 0, 0, 3],
    [2, 0, 3, 4, 0, 2, 0, 0, 4, 0],
    [5, 0, 2, 0, 6, 0, 0, 0, 0, 0],
    [0, 1, 3, 0, 1, 0, 0, 4, 0, 3],
    [6, 7, 0, 3, 0, 1, 4, 0, 0, 1],
    [4, 0, 3, 0, 4, 0, 0, 0, 0, 3],
    [0, 1, 0, 2, 0, 6, 2, 0, 2, 1]]

regions :: [[Int]]
regions = [
    [1, 5, 5, 5, 9, 9, 9, 9, 20, 20],
    [1, 1, 1, 5, 6, 6, 13, 13, 20, 13],
    [2, 2, 1, 6, 6, 12, 14, 13, 13, 13],
    [2, 2, 6, 6, 10, 12, 14, 14, 14, 18],
    [2, 2, 2, 6, 10, 10, 15, 18, 18, 18],
    [3, 3, 7, 7, 7, 10, 16, 16, 21, 21],
    [3, 3, 3, 7, 7, 11, 17, 19, 21, 21],
    [4, 4, 3, 7, 11, 11, 17, 19, 22, 22],
    [4, 4, 8, 8, 8, 8, 17, 19, 19, 23],
    [4, 4, 4, 8, 8, 8, 17, 17, 23, 23]]

solution :: [[Int]]
solution = [
    [5, 3, 2, 4, 2, 4, 3, 1, 3, 1],
    [2, 4, 3, 1, 3, 5, 6, 5, 2, 4],
    [7, 5, 1, 7, 1, 2, 3, 1, 3, 2],
    [6, 4, 2, 6, 4, 1, 2, 4, 1, 3],
    [2, 1, 3, 4, 3, 2, 1, 2, 4, 1],
    [5, 6, 2, 5, 6, 1, 2, 1, 2, 4],
    [4, 1, 3, 4, 1, 3, 5, 4, 1, 3],
    [6, 7, 2, 3, 2, 1, 4, 3, 2, 1],
    [4, 2, 3, 5, 4, 7, 3, 2, 1, 3],
    [3, 1, 5, 2, 1, 6, 2, 1, 2, 1]]

main :: IO ()
main = do
  -- putStrLn (show (countCellsInRegion (regions) 1 0 0 0))
  -- putStrLn (show (usedInSameRegionRecursive inputArr 0 0 5 1 0 0 regions))
  -- putStrLn (show (isBiggerThanRegionSize 6 5))
  -- putStrLn (show (usedInSameRegionOrIsBiggerThanRegionSize inputArr 0 0 6 regions))
  -- putStrLn (show (usedInOrthogonalCell inputArr 0 0 5))
  -- putStrLn (show (topCellInTheSameRegionIsLowerOrBottomIsBiggerIfExists inputArr 1 0 6 regions))
  putStrLn (show (solveKojun inputArr regions == Just solution))