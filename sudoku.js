// A Utility Function to print the Grid
function printGrid(arr) {
  for (let i = 0; i < 9; i++) {
    for (let j = 0; j < 9; j++) {
      console.log(arr[i][j] + " ");
    }
    console.log("");
  }
}

// Function to Find the entry in
// the Grid that is still not used
// Searches the grid to find an
// entry that is still unassigned. If
// found, the reference parameters
// row, col will be set the location
// that is unassigned, and true is
// returned. If no unassigned entries
// remains, false is returned.
// 'l' is a list variable that has
// been passed from the solveSudoku function
// to keep track of incrementation
// of Rows and Columns
function findEmptyLocation(arr, l) {
  for (let row = 0; row < 9; row++) {
    for (let col = 0; col < 9; col++) {
      if (arr[row][col] === 0) {
        l[0] = row;
        l[1] = col;
        return true;
      }
    }
  }
  return false;
}

// Returns a boolean which indicates
// whether any assigned entry
// in the specified row matches
// the given number.
function usedInRow(arr, row, num) {
  for (let i = 0; i < 9; i++) {
    if (arr[row][i] === num) {
      return true;
    }
  }
  return false;
}

// Returns a boolean which indicates
// whether any assigned entry
// in the specified column matches
// the given number.
function usedInCol(arr, col, num) {
  for (let i = 0; i < 9; i++) {
    if (arr[i][col] === num) {
      return true;
    }
  }
  return false;
}

// Returns a boolean which indicates
// whether any assigned entry
// within the specified 3x3 box
// matches the given number
function usedInBox(arr, row, col, num) {
  for (let i = 0; i < 3; i++) {
    for (let j = 0; j < 3; j++) {
      if (arr[i + row][j + col] === num) {
        return true;
      }
    }
  }
  return false;
}

// Checks whether it will be legal
// to assign num to the given row, col
// Returns a boolean which indicates
// whether it will be legal to assign
// num to the given row, col location.
function checkLocationIsSafe(arr, row, col, num) {
  // Check if 'num' is not already
  // placed in current row,
  // current column and current 3x3 box
  return (
    !usedInRow(arr, row, num) &&
    !usedInCol(arr, col, num) &&
    !usedInBox(arr, row - (row % 3), col - (col % 3), num)
  );
}

// Takes a partially filled-in grid
// and attempts to assign values to
// all unassigned locations in such a
// way to meet the requirements
// for Sudoku solution (non-duplication
// across rows, columns, and boxes)
function solveSudoku(arr) {
  // 'l' is a list variable that keeps the
  // record of row and col in
  // findEmptyLocation Function
  let l = [0, 0];

  // If there is no unassigned
  // location, we are done
  if (!findEmptyLocation(arr, l)) {
    return true;
  }

  // Assigning list values to row and col
  // that we got from the above Function
  let row = l[0];
  let col = l[1];

  // consider digits 1 to 9
  for (let num = 1; num <= 9; num++) {
    // if looks promising
    if (checkLocationIsSafe(arr, row, col, num)) {
      // make tentative assignment
      arr[row][col] = num;

      // return, if success,
      // ya !
      if (solveSudoku(arr)) {
        return true;
      }

      // failure, unmake & try again
      arr[row][col] = 0;
    }
  }

  // this triggers backtracking
  return false;
}

// function main(params) {
//   // creating a 2D array for the grid
//   let grid = new Array(9);
//   for (let i = 0; i < 9; i++) {
//     grid[i] = new Array(9).fill(0);
//   }
//   // assigning values to the grid
//   grid = [
//     [3, 0, 6, 5, 0, 8, 4, 0, 0],
//     [5, 2, 0, 0, 0, 0, 0, 0, 0],
//     [0, 8, 7, 0, 0, 0, 0, 3, 1],
//     [0, 0, 3, 0, 1, 0, 0, 8, 0],
//     [9, 0, 0, 8, 6, 3, 0, 0, 5],
//     [0, 5, 0, 0, 9, 0, 6, 0, 0],
//     [1, 3, 0, 0, 0, 0, 2, 5, 0],
//     [0, 0, 0, 0, 0, 0, 0, 7, 4],
//     [0, 0, 5, 2, 0, 6, 3, 0, 0]
//   ];

//   // if success print the grid
//   if (solveSudoku(grid)) {
//     printGrid(grid);
//   } else {
//     console.log("No solution exists");
//   }
// }

// main();

module.exports = solveSudoku;
