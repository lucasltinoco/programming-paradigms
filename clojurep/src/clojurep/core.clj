(ns clojurep.core)

(defn find-empty-location-recursive [arr location]
  (let [[row col] location]
    (cond
      (>= row (count arr)) [false location]
      (>= col (count (first arr))) (find-empty-location-recursive arr [(inc row) 0])
      (= (get-in arr [row col]) 0) [true location]
      :else (find-empty-location-recursive arr [row (inc col)]))))

;; Encontra a primeira célula vazia
(defn find-empty-location [arr location]
  (let [[found found-location] (find-empty-location-recursive arr location)]
    (if found
      found-location
      [-1])))

;; Verifica se o número já foi usado na mesma região
(defn used-in-same-region-recursive [arr row col num num-region i j regions-arr]
  (cond
    (>= i (count arr)) false
    (>= j (count (first arr))) (used-in-same-region-recursive arr row col num num-region (inc i) 0 regions-arr)
    (and (= i row) (= j col)) (used-in-same-region-recursive arr row col num num-region i (inc j) regions-arr)
    (and (= (get-in arr [i j]) num) (= (get-in regions-arr [i j]) num-region)) true
    :else (used-in-same-region-recursive arr row col num num-region i (inc j) regions-arr)))

;; Verifica se o número é maior que o tamanho da região
(defn is-bigger-than-region-size [num region-cells-count]
  (< region-cells-count num))

;; Conta o número de células na região
(defn count-cells-in-region [regions-arr num-region row col cells-count]
  (cond
    (>= row (count regions-arr)) cells-count
    (>= col (count (first regions-arr))) (count-cells-in-region regions-arr num-region (inc row) 0 cells-count)
    (= (get-in regions-arr [row col]) num-region) (count-cells-in-region regions-arr num-region row (inc col) (inc cells-count))
    :else (count-cells-in-region regions-arr num-region row (inc col) cells-count)))

;; Verifica se o número já foi usado na mesma região ou se é maior que o tamanho da região
(defn used-in-same-region-or-is-bigger-than-region-size [arr row col num regions-arr]
  (let [num-region (get-in regions-arr [row col])
        region-cells-count (count-cells-in-region regions-arr num-region 0 0 0)]
    (or (used-in-same-region-recursive arr row col num num-region 0 0 regions-arr)
        (is-bigger-than-region-size num region-cells-count))))

;; Verifica se o número já foi usado em uma célula ortogonal
(defn used-in-orthogonal-cell [arr row col num]
  (let [top-is-equal (and (> row 0) (= (get-in arr [(dec row) col]) num))
        bottom-is-equal (and (< row (dec (count arr))) (= (get-in arr [(inc row) col]) num))
        left-is-equal (and (> col 0) (= (get-in arr [row (dec col)]) num))
        right-is-equal (and (< col (dec (count (first arr)))) (= (get-in arr [row (inc col)]) num))]
    (or top-is-equal bottom-is-equal left-is-equal right-is-equal)))

;; Verifica se a célula de cima é menor ou se a de baixo é maior
(defn top-cell-in-the-same-region-is-lower-or-bottom-is-bigger-if-exists [arr row col num regions-arr]
  (let [num-region (get-in regions-arr [row col])
        top-num (if (> row 0) (get-in arr [(dec row) col]) nil)
        top-num-region (if (> row 0) (get-in regions-arr [(dec row) col]) nil)
        bottom-num (if (< row (dec (count arr))) (get-in arr [(inc row) col]) nil)
        bottom-num-region (if (< row (dec (count arr))) (get-in regions-arr [(inc row) col]) nil)]
    (or (and (= top-num-region num-region) (< top-num num))
        (and (= bottom-num-region num-region) (> bottom-num num)))))

;; Verifica se a célula é segura para ser preenchida
(defn check-location-is-safe [arr row col num regions-arr]
  (and (not (used-in-same-region-or-is-bigger-than-region-size arr row col num regions-arr))
       (not (used-in-orthogonal-cell arr row col num))
       (not (top-cell-in-the-same-region-is-lower-or-bottom-is-bigger-if-exists arr row col num regions-arr))))

;; Gera uma matriz atualizada com o número na posição especificada
(defn generate-updated-matrix [matrix row col num]
  (let [updated-row (assoc (nth matrix row) col num)]
    (assoc matrix row updated-row)))

(declare solve-kojun)

;; Preenche a célula com o número especificado
(defn fill-cell-with-number [arr row col num regions-arr]
  (if (> num (count arr))
    nil
    (let [location-is-safe (check-location-is-safe arr row col num regions-arr)]
      (if location-is-safe
        (let [result-array
              (solve-kojun (generate-updated-matrix arr row col num) regions-arr)]
          (if result-array
            result-array
            (fill-cell-with-number (generate-updated-matrix arr row col 0) row col (inc num) regions-arr)))
        (fill-cell-with-number (generate-updated-matrix arr row col 0) row col (inc num) regions-arr)))))

;; Resolve o desafio Kojun: função principal
(defn solve-kojun [arr regions-arr]
  (let [empty-location (find-empty-location arr [0 0])]
    (if (= 2 (count empty-location))
      (fill-cell-with-number arr (nth empty-location 0) (nth empty-location 1) 1 regions-arr)
      arr)))

(defn -main []
  (let [result (solve-kojun
                [[2 0 0 0 1 0]
                 [0 0 0 3 0 0]
                 [0 3 0 0 5 3]
                 [0 0 0 0 0 0]
                 [0 0 3 0 4 2]
                 [0 0 0 0 0 0]]
                [[1 1 7 7 7 11]
                 [2 2 2 2 2 11]
                 [3 6 6 6 2 10]
                 [3 3 3 6 10 10]
                 [4 4 8 9 9 9]
                 [5 5 8 8 9 9]])]
    (println "Result:" result)))
