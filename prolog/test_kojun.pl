:- consult('kojun.pl').

% Definição do tabuleiro.
board(6, [[(01-2),(01-_),(07-_),(07-_),(07-1),(11-_)],
           [(02-_),(02-_),(02-_),(02-3),(02-_),(11-_)],
           [(03-_),(06-3),(06-_),(06-_),(02-5),(10-3)],
           [(03-_),(03-_),(03-_),(06-_),(10-_),(10-_)],
           [(04-_),(04-_),(08-3),(09-_),(09-4),(09-2)],
           [(05-_),(05-_),(08-_),(08-_),(09-_),(09-_)]]).

board(8, [ [(01-2),(04-5),(04-_),(04-_),(04-3),(12-_),(14-_),(14-_)],
           [(01-_),(01-_),(07-6),(04-_),(12-_),(12-_),(15-_),(15-_)],
           [(02-_),(05-_),(07-5),(10-_),(12-5),(12-2),(16-_),(16-_)],
           [(02-_),(06-_),(07-_),(07-2),(07-_),(12-_),(16-_),(16-_)],
           [(02-_),(06-_),(07-1),(11-_),(11-4),(11-_),(16-_),(17-_)],
           [(03-3),(06-_),(08-2),(11-_),(08-_),(08-4),(17-_),(17-_)],
           [(03-_),(03-_),(08-_),(08-6),(08-_),(13-_),(13-_),(17-_)],
           [(03-_),(03-_),(09-_),(09-_),(13-4),(13-_),(13-3),(17-2)]]).

board(10, [ [(00-5), (01-_), (01-2), (01-_), (02-2), (02-_), (02-3), (02-1), (03-3), (03-1)],
            [(00-_), (00-4), (00-_), (01-1), (04-_), (04-5), (05-_), (05-5), (03-_), (05-4)],
            [(06-7), (06-5), (00-1), (04-7), (04-_), (07-_), (08-3), (05-1), (05-3), (05-_)],
            [(06-_), (06-4), (04-_), (04-_), (09-_), (07-_), (08-_), (08-_), (08-_), (10-3)],
            [(06-2), (06-_), (06-3), (04-4), (09-_), (09-2), (11-_), (10-_), (10-4), (10-_)],
            [(12-5), (12-_), (13-2), (13-_), (13-6), (09-_), (14-_), (14-_), (15-_), (15-_)],
            [(12-_), (12-1), (12-3), (13-_), (13-1), (16-_), (17-_), (18-4), (15-_), (15-3)],
            [(19-6), (19-7), (12-_), (13-3), (16-_), (16-1), (17-4), (18-_), (20-_), (20-1)],
            [(19-4), (19-_), (21-3), (21-_), (21-4), (21-_), (17-_), (18-_), (18-_), (22-3)],
            [(19-_), (19-1), (19-_), (21-2), (21-_), (21-6), (17-2), (17-_), (22-2), (22-1)]]).

board(12, [
  [
    (1-2),  (1-_),
    (1-6),  (1-3),
    (12-5), (12-4),
    (12-_), (22-_),
    (22-3), (22-_),
    (30-_), (30-2)
  ],
  [
    (1-_),  (1-4),
    (1-_),  (12-_),
    (12-_), (17-_),
    (18-_), (18-_),
    (25-_), (27-_),
    (27-3), (32-_)
  ],
  [
    (2-_),  (2-1),
    (2-_),  (9-4),
    (12-2), (18-3),
    (18-_), (18-4),
    (25-_), (27-_),
    (27-1), (32-_)
  ],
  [
    (2-_),  (3-_),
    (9-6),  (9-_),
    (9-7),  (9-_),
    (19-7), (23-_),
    (23-2), (26-7),
    (26-_), (32-_)
  ],
  [
    (2-_),  (3-2),
    (10-_), (9-_),
    (15-_), (15-2),
    (19-5), (19-4),
    (19-_), (28-_),
    (26-_), (32-_)
  ],
  [
    (3-_),  (3-_),
    (6-_),  (9-_),
    (15-3), (15-_),
    (19-_), (19-1),
    (19-3), (28-_),
    (26-_), (33-_)
  ],
  [
    (3-4),  (6-2),
    (6-_),  (6-6),
    (16-5), (16-_),
    (20-5), (20-_),
    (26-_), (26-2),
    (26-_), (33-_)
  ],
  [
    (4-7),  (4-6),
    (4-_),  (6-4),
    (16-_), (16-2),
    (20-_), (20-3),
    (20-7), (20-6),
    (31-5), (31-_)
  ],
  [
    (4-_),  (4-_),
    (4-_),  (6-_),
    (16-_), (13-_),
    (21-_), (21-4),
    (21-_), (20-_),
    (31-_), (31-_)
  ],
  [
    (5-_),  (7-_),
    (4-_),  (13-7),
    (13-4), (13-3),
    (13-_), (13-6),
    (21-_), (21-_),
    (31-3), (34-_)
  ],
  [
    (5-_),  (5-_),
    (11-3), (11-_),
    (11-_), (11-5),
    (11-_), (13-_),
    (24-_), (29-_),
    (29-_), (34-_)
  ],
  [
    (5-_),  (8-_),
    (8-_),  (14-2),
    (14-4), (14-_),
    (14-1), (24-_),
    (24-_), (24-4),
    (29-1), (29-_)
  ]
]).

board(14, [
    [
      (1-3),  (1-1),
      (1-2),  (1-6),
      (1-_),  (18-4),
      (18-2), (24-_),
      (29-5), (29-_),
      (29-2), (36-4),
      (36-_), (44-4)
    ],
    [
      (2-_),  (2-4),
      (2-_),  (1-_),
      (1-4),  (18-_),
      (18-_), (18-_),
      (30-_), (29-6),
      (36-_), (36-2),
      (44-_), (44-2)
    ],
    [
      (2-_),  (3-_),
      (2-_),  (10-_),
      (10-3), (10-5),
      (10-4), (21-_),
      (30-_), (29-3),
      (37-_), (37-7),
      (37-6), (44-1)
    ],
    [
      (3-2),  (3-_),
      (10-_), (10-6),
      (13-7), (10-_),
      (21-1), (21-_),
      (29-_), (29-_),
      (38-_), (37-3),
      (37-4), (45-_)
    ],
    [
      (3-1),  (3-_),
      (4-4),  (13-3),
      (13-_), (13-6),
      (22-_), (25-_),
      (25-6), (25-_),
      (38-_), (37-_),
      (37-_), (45-_)
    ],
    [
      (4-5),  (4-_),
      (4-_),  (4-7),
      (13-_), (13-4),
      (22-_), (26-3),
      (25-4), (25-_),
      (25-2), (40-_),
      (45-2), (45-_)
    ],
    [
      (5-_),  (5-4),
      (4-1),  (4-6),
      (13-_), (19-_),
      (19-3), (26-_),
      (26-_), (32-_),
      (33-_), (41-_),
      (41-_), (45-4)
    ],
    [
      (5-2),  (5-_),
      (5-_),  (14-_),
      (15-_), (19-1),
      (15-_), (27-_),
      (27-5), (33-_),
      (33-2), (42-_),
      (41-_), (45-_)
    ],
    [
      (6-6),  (8-2),
      (8-_),  (11-_),
      (15-4), (15-_),
      (15-_), (27-3),
      (27-_), (33-3),
      (33-_), (42-_),
      (42-6), (45-_)
    ],
    [
      (6-4),  (6-_),
      (11-2), (11-5),
      (11-_), (11-6),
      (23-_), (27-1),
      (27-_), (34-_),
      (35-4), (42-_),
      (42-2), (42-7)
    ],
    [
      (6-_),  (6-3),
      (11-_), (11-_),
      (16-_), (17-_),
      (23-_), (20-_),
      (31-5), (35-3),
      (35-_), (35-_),
      (46-_), (42-4)
    ],
    [
      (6-_),  (9-_),
      (9-5),  (12-_),
      (17-_), (17-_),
      (20-3), (20-_),
      (31-3), (31-_),
      (39-_), (35-_),
      (35-5), (47-_)
    ],
    [
      (7-_),  (9-_),
      (9-_),  (12-_),
      (12-3), (20-_),
      (20-_), (20-5),
      (31-_), (31-1),
      (39-2), (39-_),
      (43-_), (43-_)
    ],
    [
      (7-_),  (9-_),
      (12-6), (12-2),
      (12-_), (12-5),
      (20-_), (28-_),
      (28-1), (28-3),
      (28-_), (43-3),
      (43-_), (43-4)
    ]
]).

board(17, [
    [
      (1-_),  (2-3),  (2-_),
      (17-_), (20-_), (20-_),
      (28-5), (28-1), (28-_),
      (28-3), (44-1), (44-_),
      (44-7), (44-3), (44-_),
      (44-4), (44-2)
    ],
    [
      (2-6),  (2-_),  (2-_),
      (17-_), (17-_), (20-3),
      (28-_), (31-_), (31-_),
      (38-_), (38-2), (39-5),
      (49-_), (49-_), (49-_),
      (49-3), (61-4)
    ],
    [
      (2-5),  (2-_),  (12-_),
      (17-1), (18-5), (20-_),
      (28-2), (28-_), (31-_),
      (31-5), (31-_), (39-_),
      (49-4), (49-1), (49-_),
      (61-_), (61-_)
    ],
    [
      (3-_),  (3-_),  (12-_),
      (12-_), (18-_), (26-_),
      (29-_), (29-_), (31-_),
      (31-2), (39-7), (39-_),
      (50-6), (50-_), (50-4),
      (61-_), (62-6)
    ],
    [
      (4-6),  (9-_),  (10-_),
      (12-4), (18-3), (26-_),
      (30-_), (29-2), (29-_),
      (39-_), (39-_), (39-_),
      (50-2), (50-1), (50-_),
      (62-4), (62-_)
    ],
    [
      (4-_),  (10-_), (10-_),
      (12-_), (18-_), (21-5),
      (30-_), (30-_), (32-3),
      (40-_), (40-4), (40-_),
      (40-5), (55-_), (55-5),
      (62-_), (62-2)
    ],
    [
      (4-1),  (4-_),  (13-_),
      (18-_), (18-_), (21-3),
      (21-_), (32-_), (32-_),
      (32-5), (45-_), (40-3),
      (40-_), (55-_), (55-4),
      (57-_), (62-_)
    ],
    [
      (5-_),  (4-_),  (13-_),
      (14-_), (21-_), (21-_),
      (21-1), (33-_), (36-_),
      (32-_), (45-_), (47-_),
      (51-1), (51-_), (55-_),
      (57-_), (57-2)
    ],
    [
      (5-_),  (4-_),  (14-5),
      (14-_), (22-_), (22-_),
      (23-_), (33-_), (36-_),
      (41-_), (41-_), (47-_),
      (52-_), (52-6), (57-4),
      (57-_), (58-_)
    ],
    [
      (5-_),  (6-4),  (14-_),
      (14-_), (23-4), (23-_),
      (23-3), (33-_), (36-3),
      (42-_), (42-5), (47-_),
      (47-2), (52-_), (52-2),
      (57-_), (58-_)
    ],
    [
      (6-5),  (6-_),  (6-2),
      (15-_), (15-3), (15-5),
      (23-_), (27-6), (36-_),
      (36-_), (42-_), (48-_),
      (48-_), (53-5), (52-_),
      (52-3), (58-_)
    ],
    [
      (7-_),  (6-_),  (15-4),
      (15-2), (15-_), (27-_),
      (27-7), (27-_), (36-_),
      (42-7), (42-_), (48-_),
      (53-3), (53-_), (58-6),
      (58-5), (58-_)
    ],
    [
      (7-3),  (7-_),  (16-_),
      (19-1), (19-4), (27-_),
      (27-2), (27-4), (37-_),
      (42-_), (42-2), (48-_),
      (53-_), (53-_), (59-5),
      (59-_), (59-_)
    ],
    [
      (7-_),  (7-1),  (16-_),
      (16-6), (19-_), (19-2),
      (24-_), (34-_), (37-_),
      (37-3), (46-_), (46-5),
      (54-_), (54-_), (59-3),
      (60-6), (59-2)
    ],
    [
      (8-_),  (8-_),  (16-4),
      (16-_), (24-5), (24-6),
      (24-2), (34-_), (34-_),
      (34-_), (46-3), (46-_),
      (54-_), (54-1), (60-_),
      (60-4), (60-7)
    ],
    [
      (8-2),  (11-_), (16-2),
      (16-_), (24-4), (24-_),
      (25-_), (35-_), (35-1),
      (43-_), (43-_), (46-1),
      (54-_), (56-4), (56-_),
      (60-_), (63-_)
    ],
    [
      (8-1),  (8-7),  (8-6),
      (8-5),  (25-_), (25-4),
      (25-1), (25-7), (25-2),
      (25-3), (43-1), (43-2),
      (43-3), (56-_), (56-2),
      (60-_), (60-_)
    ]
]).

% Definição da solução.
solution(6, [[2, 1, 3, 2, 1, 2],
              [1, 4, 2, 3, 6, 1],
              [4, 3, 4, 2, 5, 3],
              [3, 1, 2, 1, 2, 1],
              [1, 2, 3, 5, 4, 2],
              [2, 1, 2, 1, 3, 1]]).

solution(8, [[2, 5, 1, 4, 3, 4, 1, 2],
            [1, 3, 6, 2, 6, 3, 2, 1],
            [3, 1, 5, 1, 5, 2, 5, 2],
            [2, 3, 4, 2, 3, 1, 4, 1],
            [1, 2, 1, 3, 4, 2, 3, 5],
            [3, 1, 2, 1, 5, 4, 1, 4],
            [2, 5, 1, 6, 3, 2, 5, 3],
            [1, 4, 2, 1, 4, 1, 3, 2]]).

solution(10, [[5, 3, 2, 4, 2, 4, 3, 1, 3, 1],
            [2, 4, 3, 1, 3, 5, 6, 5, 2, 4],
            [7, 5, 1, 7, 1, 2, 3, 1, 3, 2],
            [6, 4, 2, 6, 4, 1, 2, 4, 1, 3],
            [2, 1, 3, 4, 3, 2, 1, 2, 4, 1],
            [5, 6, 2, 5, 6, 1, 2, 1, 2, 4],
            [4, 1, 3, 4, 1, 3, 5, 4, 1, 3],
            [6, 7, 2, 3, 2, 1, 4, 3, 2, 1],
            [4, 2, 3, 5, 4, 7, 3, 2, 1, 3],
            [3, 1, 5, 2, 1, 6, 2, 1, 2, 1]]).

solution(12, [
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
]).

solution(14, [
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
]).

solution(17, [
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
]).

% Verifica se duas matrizes são iguais.
equal_matrices(Matriz1, Matriz2) :-
    length(Matriz1, Linhas),
    length(Matriz2, Linhas),
    equal_matrix_rows(Matriz1, Matriz2).

% Verifica se duas linhas são iguais.
equal_matrix_rows([], []).
equal_matrix_rows([Linha1|Resto1], [Linha2|Resto2]) :-
    equal_row(Linha1, Linha2),
    equal_matrix_rows(Resto1, Resto2).

% Verifica se dois elementos são iguais.
equal_row([], []).
equal_row([Elemento1|Resto1], [Elemento2|Resto2]) :-
    Elemento1 = Elemento2,
    equal_row(Resto1, Resto2).

% Testa a igualdade entre o tabuleiro e a solução.
test_matrix_equality(Linhas, Solução) :- 
    get_values(Linhas, Valores), 
    equal_matrices(Valores, Solução).

% Resolve o kojun.
solve(N) :-
    board(N, Tabuleiro),
    kojun(Tabuleiro),
    guess(Tabuleiro),
    solution(N, Solução),
    test_matrix_equality(Tabuleiro, Solução).

% Executa o teste para um tabuleiro 6x6.
test_6x6 :-
    solve(6).

% Executa o teste para um tabuleiro 8x8.
test_8x8 :-
    solve(8).

% Executa o teste para um tabuleiro 10x10.
test_10x10 :-
    solve(10).

% Executa o teste para um tabuleiro 12x12.
test_12x12 :-
    solve(12).

% Executa o teste para um tabuleiro 14x14.
test_14x14 :-
    solve(14).

% Executa o teste para um tabuleiro 17x17.
test_17x17 :-
    solve(17).

% Executa todos os testes.
run_tests :-
    test_6x6,
    test_8x8,
    test_10x10,
    test_12x12,
    test_14x14,
    test_17x17.

:- run_tests.