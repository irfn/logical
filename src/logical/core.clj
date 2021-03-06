(ns logical.core
  (:require [logical.sudoku :as sudoku]))

(defn -main
  "run all the logic examples"
  []
  (sudoku/print-board (sudoku/solve [
                 8 7 1   0 0 2   0 4 0
                 0 0 0   8 1 5   0 0 6
                 0 5 0   4 3 0   0 0 1

                 1 0 0   6 5 8   0 7 9
                 7 6 0   0 0 0   5 0 0
                 0 9 8   0 4 3   0 6 0

                 0 0 7   0 8 0   0 0 3
                 9 1 5   0 0 0   0 0 0
                 0 8 3   1 7 0   2 0 0
                 ])))
