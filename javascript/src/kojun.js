function findEmptyLocationRecursive(arr, location) {
  const [row, col] = location;

  if (row >= arr.length) {
    return false;
  }

  if (col >= arr.length) {
    location[0] = row + 1;
    location[1] = 0;
    return findEmptyLocationRecursive(arr, location);
  }

  if (arr[row][col] === 0) {
    return true;
  }

  location[1] = col + 1;
  return findEmptyLocationRecursive(arr, location);
}

function findEmptyLocation(arr, location) {
  const found = findEmptyLocationRecursive(arr, location);
  return found ? location : null;
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
    count++;
  }
  return countCellsInRegion(regionsArr, numRegion, row, col + 1, count);
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
  const rightIsEqual = row < arr.length - 1 && arr[row][col + 1] == num;

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

function solveKojun(arr, regionsArr) {
  let l = [0, 0];

  if (!findEmptyLocation(arr, l)) {
    // Base case: the array is filled, return true
    return true;
  }

  let row = l[0];
  let col = l[1];

  // Define a recursive helper function that tries to fill the current cell with a number
  function fillCellWithNumber(num) {
    if (checkLocationIsSafe(arr, row, col, num, regionsArr)) {
      arr[row][col] = num;

      if (solveKojun(arr, regionsArr)) {
        // The puzzle is solved, return true
        return true;
      }

      arr[row][col] = 0;
    }

    // Try the next number if the current one didn't work
    if (num < arr.length) {
      return fillCellWithNumber(num + 1);
    }

    // None of the numbers from 1 to the length of the array worked in this cell
    return false;
  }

  // Call the helper function with the initial number 1
  return fillCellWithNumber(1);
}

module.exports = solveKojun;
