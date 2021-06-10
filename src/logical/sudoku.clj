(ns logical.sudoku
  (:refer-clojure :exclude [record? ==])
  (:require
   [clojure.string :as s]
   [clojure.pprint :as pp]
   [clojure.core.logic :refer :all]
   [clojure.core.logic.fd :as fd]))

(defn valido [num]
  (fd/in num (fd/interval 1 9)))

(defn to-lvars [input]
    (vec (map #(if (zero? %) (lvar) %) input)))

(defn to-boxes [board]
  (let [rows (partition 27 board)]
    (reduce (fn [boxes, box-row]
              (let [tuples (partition 3 box-row)
                    ntupl  (partial nth tuples)]
                (conj boxes
                      [(ntupl 0) (ntupl 3) (ntupl 6)]
                      [(ntupl 1) (ntupl 4) (ntupl 7)]
                      [(ntupl 2) (ntupl 5) (ntupl 8)])))
            [] rows)))

(defn col [rows num]
  (map #(nth % num) rows))

(defn rows [board]
  (partition 9 board))

(defn print-board [board]
  (doall (map #(println (s/join " " %)) (rows board))))

(defn solve [input]
  (let [board (to-lvars input)
        rows  (rows board)
        cols  (map #(col rows %) (range 0 8))
        boxes (to-boxes board)
        ]
    (first
     (run 1 [q]
       (== q board)
       (everyg #(valido %) board)
       (everyg fd/distinct rows)
       (everyg fd/distinct cols)
       (everyg fd/distinct (map #(reduce concat %) boxes)))
       )))
