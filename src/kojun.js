function findEmptyLocation(arr, l) {
  for (let row = 0; row < 10; row++) {
    for (let col = 0; col < 10; col++) {
      if (arr[row][col] === 0) {
        l[0] = row;
        l[1] = col;
        return true;
      }
    }
  }
  return false;
}

function usedInSameRegionOrIsBiggerThanRegionSize(
  arr,
  row,
  col,
  num,
  regionsArr
) {
  const numRegion = regionsArr[row][col];
  let regionCellsCount = 0;
  for (let row = 0; row < 10; row++) {
    for (let col = 0; col < 10; col++) {
      const loopNum = arr[row][col];
      const loopNumRegion = regionsArr[row][col];
      if (loopNum === num && loopNumRegion === numRegion) {
        return true;
      }
      if (regionsArr[row][col] === numRegion) {
        regionCellsCount++;
      }
    }
  }
  if (regionCellsCount < num) {
    return true;
  }
  return false;
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
    return true;
  }

  let row = l[0];
  let col = l[1];

  for (let num = 1; num <= 10; num++) {
    if (checkLocationIsSafe(arr, row, col, num, regionsArr)) {
      arr[row][col] = num;

      if (solveKojun(arr, regionsArr)) {
        return true;
      }

      arr[row][col] = 0;
    }
  }

  return false;
}

module.exports = solveKojun;
