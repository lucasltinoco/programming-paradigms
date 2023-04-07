const solveKojun = require("../kojun");

test("10x10", () => {
  let valuesInputGrid = [
    [5, 0, 2, 0, 2, 0, 3, 1, 3, 1],
    [0, 4, 0, 1, 0, 5, 0, 5, 0, 4],
    [7, 5, 1, 7, 0, 0, 3, 1, 3, 0],
    [0, 4, 0, 0, 0, 0, 0, 0, 0, 3],
    [2, 0, 3, 4, 0, 2, 0, 0, 4, 0],
    [5, 0, 2, 0, 6, 0, 0, 0, 0, 0],
    [0, 1, 3, 0, 1, 0, 0, 4, 0, 3],
    [6, 7, 0, 3, 0, 1, 4, 0, 0, 1],
    [4, 0, 3, 0, 4, 0, 0, 0, 0, 3],
    [0, 1, 0, 2, 0, 6, 2, 0, 2, 1]
  ];
  const regionsInputGrid = [
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
  ];
  const solution = [
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
  ];

  solveKojun(valuesInputGrid, regionsInputGrid);
  expect(valuesInputGrid).toStrictEqual(solution);
});

test("12x12", () => {
  let valuesInputGrid = [
    [2, 0, 6, 3, 5, 4, 0, 0, 3, 0, 0, 2],
    [0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0],
    [0, 1, 0, 4, 2, 3, 0, 4, 0, 0, 1, 0],
    [0, 0, 6, 0, 7, 0, 7, 0, 2, 7, 0, 0],
    [0, 2, 0, 0, 0, 2, 5, 4, 0, 0, 0, 0],
    [0, 0, 0, 0, 3, 0, 0, 1, 3, 0, 0, 0],
    [4, 2, 0, 6, 5, 0, 5, 0, 0, 2, 0, 0],
    [7, 6, 0, 4, 0, 2, 0, 3, 7, 6, 5, 0],
    [0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0],
    [0, 0, 0, 7, 4, 3, 0, 6, 0, 0, 3, 0],
    [0, 0, 3, 0, 0, 5, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 2, 4, 0, 1, 0, 0, 4, 1, 0]
  ];
  const regionsInputGrid = [
    [1, 1, 1, 1, 12, 12, 12, 22, 22, 22, 30, 30],
    [1, 1, 1, 12, 12, 17, 18, 18, 25, 27, 27, 32],
    [2, 2, 2, 9, 12, 18, 18, 18, 25, 27, 27, 32],
    [2, 3, 9, 9, 9, 9, 19, 23, 23, 26, 26, 32],
    [2, 3, 10, 9, 15, 15, 19, 19, 19, 28, 26, 32],
    [3, 3, 6, 9, 15, 15, 19, 19, 19, 28, 26, 33],
    [3, 6, 6, 6, 16, 16, 20, 20, 26, 26, 26, 33],
    [4, 4, 4, 6, 16, 16, 20, 20, 20, 20, 31, 31],
    [4, 4, 4, 6, 16, 13, 21, 21, 21, 20, 31, 31],
    [5, 7, 4, 13, 13, 13, 13, 13, 21, 21, 31, 34],
    [5, 5, 11, 11, 11, 11, 11, 13, 24, 29, 29, 34],
    [5, 8, 8, 14, 14, 14, 14, 24, 24, 24, 29, 29]
  ];
  const solution = [
    [2, 7, 6, 3, 5, 4, 6, 1, 3, 2, 1, 2],
    [1, 4, 5, 1, 3, 1, 2, 5, 2, 4, 3, 4],
    [5, 1, 2, 4, 2, 3, 1, 4, 1, 2, 1, 3],
    [4, 3, 6, 3, 7, 5, 7, 1, 2, 7, 6, 2],
    [3, 2, 1, 2, 4, 2, 5, 4, 6, 2, 5, 1],
    [5, 1, 5, 1, 3, 1, 2, 1, 3, 1, 4, 2],
    [4, 2, 1, 6, 5, 4, 5, 4, 1, 2, 3, 1],
    [7, 6, 5, 4, 3, 2, 1, 3, 7, 6, 5, 2],
    [1, 3, 4, 3, 1, 5, 3, 4, 5, 2, 4, 1],
    [3, 1, 2, 7, 4, 3, 2, 6, 2, 1, 3, 2],
    [2, 4, 3, 1, 2, 5, 4, 1, 3, 2, 4, 1],
    [1, 2, 1, 2, 4, 3, 1, 2, 1, 4, 1, 3]
  ];

  solveKojun(valuesInputGrid, regionsInputGrid);
  expect(valuesInputGrid).toStrictEqual(solution);
});

test("14x14", () => {
  let valuesInputGrid = [
    [3, 1, 2, 6, 0, 4, 2, 0, 5, 0, 2, 4, 0, 4],
    [0, 4, 0, 0, 4, 0, 0, 0, 0, 6, 0, 2, 0, 2],
    [0, 0, 0, 0, 3, 5, 4, 0, 0, 3, 0, 7, 6, 1],
    [2, 0, 0, 6, 7, 0, 1, 0, 0, 0, 0, 3, 4, 0],
    [1, 0, 4, 3, 0, 6, 0, 0, 6, 0, 0, 0, 0, 0],
    [5, 0, 0, 7, 0, 4, 0, 3, 4, 0, 2, 0, 2, 0],
    [0, 4, 1, 6, 0, 0, 3, 0, 0, 0, 0, 0, 0, 4],
    [2, 0, 0, 0, 0, 1, 0, 0, 5, 0, 2, 0, 0, 0],
    [6, 2, 0, 0, 4, 0, 0, 3, 0, 3, 0, 0, 6, 0],
    [4, 0, 2, 5, 0, 6, 0, 1, 0, 0, 4, 0, 2, 7],
    [0, 3, 0, 0, 0, 0, 0, 0, 5, 3, 0, 0, 0, 4],
    [0, 0, 5, 0, 0, 0, 3, 0, 3, 0, 0, 0, 5, 0],
    [0, 0, 0, 0, 3, 0, 0, 5, 0, 1, 2, 0, 0, 0],
    [0, 0, 6, 2, 0, 5, 0, 0, 1, 3, 0, 3, 0, 4]
  ];
  const regionsInputGrid = [
    [1, 1, 1, 1, 1, 18, 18, 24, 29, 29, 29, 36, 36, 44],
    [2, 2, 2, 1, 1, 18, 18, 18, 30, 29, 36, 36, 44, 44],
    [2, 3, 2, 10, 10, 10, 10, 21, 30, 29, 37, 37, 37, 44],
    [3, 3, 10, 10, 13, 10, 21, 21, 29, 29, 38, 37, 37, 45],
    [3, 3, 4, 13, 13, 13, 22, 25, 25, 25, 38, 37, 37, 45],
    [4, 4, 4, 4, 13, 13, 22, 26, 25, 25, 25, 40, 45, 45],
    [5, 5, 4, 4, 13, 19, 19, 26, 26, 32, 33, 41, 41, 45],
    [5, 5, 5, 14, 15, 19, 15, 27, 27, 33, 33, 42, 41, 45],
    [6, 8, 8, 11, 15, 15, 15, 27, 27, 33, 33, 42, 42, 45],
    [6, 6, 11, 11, 11, 11, 23, 27, 27, 34, 35, 42, 42, 42],
    [6, 6, 11, 11, 16, 17, 23, 20, 31, 35, 35, 35, 46, 42],
    [6, 9, 9, 12, 17, 17, 20, 20, 31, 31, 39, 35, 35, 47],
    [7, 9, 9, 12, 12, 20, 20, 20, 31, 31, 39, 39, 43, 43],
    [7, 9, 12, 12, 12, 12, 20, 28, 28, 28, 28, 43, 43, 43]
  ];
  const solution = [
    [3, 1, 2, 6, 7, 4, 2, 1, 5, 7, 2, 4, 1, 4],
    [5, 4, 3, 5, 4, 3, 1, 5, 2, 6, 3, 2, 3, 2],
    [1, 5, 2, 7, 3, 5, 4, 3, 1, 3, 5, 7, 6, 1],
    [2, 4, 1, 6, 7, 2, 1, 2, 4, 1, 2, 3, 4, 7],
    [1, 3, 4, 3, 5, 6, 2, 1, 6, 5, 1, 2, 1, 6],
    [5, 2, 3, 7, 2, 4, 1, 3, 4, 3, 2, 1, 2, 5],
    [3, 4, 1, 6, 1, 2, 3, 1, 2, 1, 5, 2, 3, 4],
    [2, 1, 5, 1, 5, 1, 2, 6, 5, 4, 2, 5, 1, 3],
    [6, 2, 1, 7, 4, 3, 1, 3, 4, 3, 1, 3, 6, 1],
    [4, 5, 2, 5, 3, 6, 2, 1, 2, 1, 4, 1, 2, 7],
    [2, 3, 1, 4, 1, 3, 1, 7, 5, 3, 1, 6, 1, 4],
    [1, 4, 5, 7, 2, 1, 3, 6, 3, 4, 3, 2, 5, 1],
    [2, 3, 1, 4, 3, 4, 2, 5, 2, 1, 2, 1, 2, 5],
    [1, 2, 6, 2, 1, 5, 1, 2, 1, 3, 4, 3, 1, 4]
  ];

  solveKojun(valuesInputGrid, regionsInputGrid);
  expect(valuesInputGrid).toStrictEqual(solution);
});

test("17x17", () => {
  let valuesInputGrid = [
    [0, 3, 0, 0, 0, 0, 5, 1, 0, 3, 1, 0, 7, 3, 0, 4, 2],
    [6, 0, 0, 0, 0, 3, 0, 0, 0, 0, 2, 5, 0, 0, 0, 3, 4],
    [5, 0, 0, 1, 5, 0, 2, 0, 0, 5, 0, 0, 4, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 7, 0, 6, 0, 4, 0, 6],
    [6, 0, 0, 4, 3, 0, 0, 2, 0, 0, 0, 0, 2, 1, 0, 4, 0],
    [0, 0, 0, 0, 0, 5, 0, 0, 3, 0, 4, 0, 5, 0, 5, 0, 2],
    [1, 0, 0, 0, 0, 3, 0, 0, 0, 5, 0, 3, 0, 0, 4, 0, 0],
    [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2],
    [0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 4, 0, 0],
    [0, 4, 0, 0, 4, 0, 3, 0, 3, 0, 5, 0, 2, 0, 2, 0, 0],
    [5, 0, 2, 0, 3, 5, 0, 6, 0, 0, 0, 0, 0, 5, 0, 3, 0],
    [0, 0, 4, 2, 0, 0, 7, 0, 0, 7, 0, 0, 3, 0, 6, 5, 0],
    [3, 0, 0, 1, 4, 0, 2, 4, 0, 0, 2, 0, 0, 0, 5, 0, 0],
    [0, 1, 0, 6, 0, 2, 0, 0, 0, 3, 0, 5, 0, 0, 3, 6, 2],
    [0, 0, 4, 0, 5, 6, 2, 0, 0, 0, 3, 0, 0, 1, 0, 4, 7],
    [2, 0, 2, 0, 4, 0, 0, 0, 1, 0, 0, 1, 0, 4, 0, 0, 0],
    [1, 7, 6, 5, 0, 4, 1, 7, 2, 3, 1, 2, 3, 0, 2, 0, 0]
  ];
  const regionsInputGrid = [
    [1, 2, 2, 17, 20, 20, 28, 28, 28, 28, 44, 44, 44, 44, 44, 44, 44],
    [2, 2, 2, 17, 17, 20, 28, 31, 31, 38, 38, 39, 49, 49, 49, 49, 61],
    [2, 2, 12, 17, 18, 20, 28, 28, 31, 31, 31, 39, 49, 49, 49, 61, 61],
    [3, 3, 12, 12, 18, 26, 29, 29, 31, 31, 39, 39, 50, 50, 50, 61, 62],
    [4, 9, 10, 12, 18, 26, 30, 29, 29, 39, 39, 39, 50, 50, 50, 62, 62],
    [4, 10, 10, 12, 18, 21, 30, 30, 32, 40, 40, 40, 40, 55, 55, 62, 62],
    [4, 4, 13, 18, 18, 21, 21, 32, 32, 32, 45, 40, 40, 55, 55, 57, 62],
    [5, 4, 13, 14, 21, 21, 21, 33, 36, 32, 45, 47, 51, 51, 55, 57, 57],
    [5, 4, 14, 14, 22, 22, 23, 33, 36, 41, 41, 47, 52, 52, 57, 57, 58],
    [5, 6, 14, 14, 23, 23, 23, 33, 36, 42, 42, 47, 47, 52, 52, 57, 58],
    [6, 6, 6, 15, 15, 15, 23, 27, 36, 36, 42, 48, 48, 53, 52, 52, 58],
    [7, 6, 15, 15, 15, 27, 27, 27, 36, 42, 42, 48, 53, 53, 58, 58, 58],
    [7, 7, 16, 19, 19, 27, 27, 27, 37, 42, 42, 48, 53, 53, 59, 59, 59],
    [7, 7, 16, 16, 19, 19, 24, 34, 37, 37, 46, 46, 54, 54, 59, 60, 59],
    [8, 8, 16, 16, 24, 24, 24, 34, 34, 34, 46, 46, 54, 54, 60, 60, 60],
    [8, 11, 16, 16, 24, 24, 25, 35, 35, 43, 43, 46, 54, 56, 56, 60, 63],
    [8, 8, 8, 8, 25, 25, 25, 25, 25, 25, 43, 43, 43, 56, 56, 60, 60]
  ];
  const solution = [
    [1, 3, 7, 3, 2, 4, 5, 1, 6, 3, 1, 6, 7, 3, 5, 4, 2],
    [6, 2, 4, 2, 4, 3, 4, 3, 7, 1, 2, 5, 6, 2, 7, 3, 4],
    [5, 1, 2, 1, 5, 1, 2, 7, 6, 5, 1, 3, 4, 1, 5, 2, 3],
    [1, 2, 1, 5, 4, 2, 4, 3, 4, 2, 7, 2, 6, 5, 4, 1, 6],
    [6, 1, 2, 4, 3, 1, 3, 2, 1, 4, 6, 1, 2, 1, 3, 4, 5],
    [2, 3, 1, 3, 2, 5, 2, 1, 3, 1, 4, 6, 5, 2, 5, 3, 2],
    [1, 5, 2, 6, 1, 3, 4, 2, 1, 5, 2, 3, 2, 1, 4, 6, 1],
    [3, 4, 1, 4, 6, 2, 1, 3, 6, 4, 1, 4, 1, 2, 3, 5, 2],
    [2, 3, 5, 3, 2, 1, 5, 2, 4, 1, 2, 3, 5, 6, 4, 3, 4],
    [1, 4, 1, 2, 4, 2, 3, 1, 3, 6, 5, 1, 2, 4, 2, 1, 3],
    [5, 3, 2, 6, 3, 5, 1, 6, 2, 5, 4, 3, 4, 5, 1, 3, 2],
    [4, 1, 4, 2, 1, 3, 7, 5, 1, 7, 3, 2, 3, 4, 6, 5, 1],
    [3, 5, 7, 1, 4, 1, 2, 4, 2, 1, 2, 1, 2, 1, 5, 1, 4],
    [2, 1, 5, 6, 3, 2, 3, 2, 1, 3, 4, 5, 4, 5, 3, 6, 2],
    [4, 3, 4, 3, 5, 6, 2, 1, 3, 4, 3, 2, 3, 1, 5, 4, 7],
    [2, 1, 2, 1, 4, 1, 5, 2, 1, 5, 4, 1, 2, 4, 3, 2, 1],
    [1, 7, 6, 5, 6, 4, 1, 7, 2, 3, 1, 2, 3, 1, 2, 1, 3]
  ];

  solveKojun(valuesInputGrid, regionsInputGrid);
  expect(valuesInputGrid).toStrictEqual(solution);
});
