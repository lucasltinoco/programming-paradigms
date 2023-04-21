module Main (main) where

findEmptyLocation :: [[Int]] -> [Int] -> [Int]
findEmptyLocation arr l = findEmptyLocationHelper arr l 0 0

findEmptyLocationHelper :: [[Int]] -> [Int] -> Int -> Int -> [Int]
findEmptyLocationHelper arr l row col
  | row == length arr = [-1, -1] -- empty location not found
  | col == length arr = findEmptyLocationHelper arr l (row + 1) 0
  | arr !! row !! col == 0 = [row, col]
  | otherwise = findEmptyLocationHelper arr l row (col + 1)

for :: (s -> s) -> (s -> Bool) -> (s -> s)
for initial condition iteration = while condition iteration initial

while :: (s -> Bool) -> (s -> s) -> (s -> s)
while condition iteration initial =
    if condition initial
        then while condition iteration (iteration initial)
        else initial

-- usedInSameRegionOrIsBiggerThanRegionSize :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Bool

usedInSameRegionOrIsBiggerThanRegionSize :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Bool
usedInSameRegionOrIsBiggerThanRegionSize arr row col num regionsArr
  | row == length arr = False
  | col == length arr = findEmptyLocationHelper arr l (row + 1) 0
  | arr !! row !! col == 0 = [row, col]
  | otherwise = findEmptyLocationHelper arr l row (col + 1)

checkLocationIsSafe :: [[Int]] -> Int -> Int -> Int -> Bool
checkLocationIsSafe _ _ _ _ = True

inputArr :: [[Int]]
inputArr = [
    [5, 3, 2, 4, 2, 4, 3, 1, 3, 1],
    [2, 4, 3, 1, 3, 5, 6, 5, 2, 4],
    [7, 5, 1, 7, 1, 2, 3, 1, 3, 2],
    [6, 4, 2, 6, 4, 1, 2, 4, 1, 3],
    [2, 1, 3, 4, 3, 2, 1, 2, 4, 1],
    [5, 6, 2, 5, 6, 1, 2, 1, 2, 4],
    [4, 1, 3, 4, 1, 3, 5, 4, 1, 3],
    [6, 7, 2, 3, 2, 1, 4, 3, 2, 1],
    [4, 2, 3, 5, 4, 7, 3, 2, 1, 3],
    [3, 1, 5, 2, 1, 6, 2, 1, 2, 1]
  ]

regionsArr :: [[Int]]
regionsArr = [
    [1, 5, 5, 5, 9, 9, 9, 9, 20, 20],
    [1, 1, 1, 5, 6, 6, 13, 13, 20, 13],
    [2, 2, 1, 6, 6, 12, 14, 13, 13, 13],
    [2, 2, 6, 6, 10, 12, 14, 14, 14, 18],
    [2, 2, 2, 6, 10, 10, 15, 18, 18, 18],
    [3, 3, 7, 7, 7, 10, 16, 16, 21, 21],
    [3, 3, 3, 7, 7, 11, 17, 19, 21, 21],
    [4, 4, 3, 7, 11, 11, 17, 19, 22, 22],
    [4, 4, 8, 8, 8, 8, 17, 19, 19, 23],
    [4, 4, 4, 8, 8, 8, 17, 17, 23, 23]
  ]

main :: IO ()
main = do
  print (for _ -> 0 (\s -> s < (length inputArray)) (\s -> s + 1))