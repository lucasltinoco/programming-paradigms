function findEmptyLocationRecursive(arr, location) {
  const [row, col] = location;

  if (row >= arr.length) {
    return [false, location];
  }

  if (col >= arr.length) {
    return findEmptyLocationRecursive(arr, [row + 1, 0]);
  }

  if (arr[row][col] === 0) {
    return [true, location];
  }

  return findEmptyLocationRecursive(arr, [row, col + 1]);
}

function findEmptyLocation(arr, location) {
  const [found, foundLocation] = findEmptyLocationRecursive(arr, location);
  return found ? foundLocation : [-1];
}

function usedInSameRegionRecursive(
  arr,
  row,
  col,
  num,
  numRegion,
  i = 0,
  j = 0,
  regionsArr
) {
  if (i >= arr.length) {
    return false;
  }
  if (j >= arr.length) {
    return usedInSameRegionRecursive(
      arr,
      row,
      col,
      num,
      numRegion,
      i + 1,
      0,
      regionsArr
    );
  }
  if (i === row && j === col) {
    return usedInSameRegionRecursive(
      arr,
      row,
      col,
      num,
      numRegion,
      i,
      j + 1,
      regionsArr
    );
  }
  if (arr[i][j] === num && regionsArr[i][j] === numRegion) {
    return true;
  }
  return usedInSameRegionRecursive(
    arr,
    row,
    col,
    num,
    numRegion,
    i,
    j + 1,
    regionsArr
  );
}

function isBiggerThanRegionSize(num, regionCellsCount) {
  return regionCellsCount < num;
}

function countCellsInRegion(
  regionsArr,
  numRegion,
  row = 0,
  col = 0,
  count = 0
) {
  if (row >= regionsArr.length) {
    return count;
  }
  if (col >= regionsArr.length) {
    return countCellsInRegion(regionsArr, numRegion, row + 1, 0, count);
  }
  if (regionsArr[row][col] === numRegion) {
    return countCellsInRegion(regionsArr, numRegion, row, col + 1, count + 1);
  }
  return countCellsInRegion(regionsArr, numRegion, row, col + 1, count);
}

function usedInSameRegionOrIsBiggerThanRegionSize(
  arr,
  row,
  col,
  num,
  regionsArr
) {
  const numRegion = regionsArr[row][col];
  const regionCellsCount = countCellsInRegion(regionsArr, numRegion);

  return (
    usedInSameRegionRecursive(
      arr,
      row,
      col,
      num,
      numRegion,
      0,
      0,
      regionsArr
    ) || isBiggerThanRegionSize(num, regionCellsCount)
  );
}

function usedInOrthogonalCell(arr, row, col, num) {
  const topIsEqual = row > 0 && arr[row - 1][col] == num;
  const bottomIsEqual = row < arr.length - 1 && arr[row + 1][col] == num;
  const leftIsEqual = col > 0 && arr[row][col - 1] == num;
  const rightIsEqual = col < arr.length - 1 && arr[row][col + 1] == num;

  return topIsEqual || bottomIsEqual || leftIsEqual || rightIsEqual;
}

function topCellInTheSameRegionIsLowerOrBottomIsBiggerIfExists(
  arr,
  row,
  col,
  num,
  regionsArr
) {
  const numRegion = regionsArr[row][col];

  const topNum = row > 0 && arr[row - 1][col];
  const topNumRegion = row > 0 && regionsArr[row - 1][col];

  const bottomNum = row < arr.length - 1 && arr[row + 1][col];
  const bottomNumRegion = row < arr.length - 1 && regionsArr[row + 1][col];

  return (
    (topNumRegion === numRegion && topNum < num) ||
    (bottomNumRegion === numRegion && bottomNum > num)
  );
}

function checkLocationIsSafe(arr, row, col, num, regionsArr) {
  return (
    !usedInSameRegionOrIsBiggerThanRegionSize(arr, row, col, num, regionsArr) &&
    !usedInOrthogonalCell(arr, row, col, num) &&
    !topCellInTheSameRegionIsLowerOrBottomIsBiggerIfExists(
      arr,
      row,
      col,
      num,
      regionsArr
    )
  );
}

function generateUpdatedMatrix(matrix, row, col, num) {
  const newMatrix = matrix.map((rowArr, rowIndex) =>
    rowIndex === row
      ? rowArr.map((item, colIndex) => (colIndex === col ? num : item))
      : rowArr
  );
  return newMatrix;
}

function fillCellWithNumber(arr, row, col, num, regionsArr) {
  if (num > arr.length) {
    return null;
  }

  const locationIsSafe = checkLocationIsSafe(arr, row, col, num, regionsArr);
  if (locationIsSafe) {
    const resultArray = solveKojun(
      generateUpdatedMatrix(arr, row, col, num),
      regionsArr
    );
    if (resultArray) {
      return resultArray;
    } else {
      return fillCellWithNumber(
        generateUpdatedMatrix(arr, row, col, 0),
        row,
        col,
        num + 1,
        regionsArr
      );
    }
  }

  return fillCellWithNumber(
    generateUpdatedMatrix(arr, row, col, 0),
    row,
    col,
    num + 1,
    regionsArr
  );
}

function solveKojun(arr, regionsArr) {
  const emptyLocation = findEmptyLocation(arr, [0, 0]);

  if (emptyLocation.length === 2) {
    return fillCellWithNumber(
      arr,
      emptyLocation[0],
      emptyLocation[1],
      1,
      regionsArr
    );
  }

  return arr;
}

module.exports = solveKojun;
