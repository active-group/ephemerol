(ns active.ephemerol.compact-table)

; Ported from Scheme 48

; A compact table is an encoding of a very large vector that has lots
; of recurring patterns.  It was written for encoding Unicode tables.

; The vector is partitioned into blocks, and the blocks get assembled
; into a new compressed vector.  Each time a new block gets added, the
; algorithm looks if the same block is already present in the
; compressed vector, or the compressed vector ends with a prefix of
; the new block.  In the former case, nothing needs to get added.  In
; the latter case, only the suffix needs to get added.  At the same
; time, the algorithm computes a table with indices of the block
; beginnings.

; The algorithm can take a long time; little attempt at optimization
; has been made.  It's mainly intended for offline computation as part
; of a build process.

; This tries to merge BLOCK onto REVERSE-BASE, sharing the prefix of
; BLOCK.

; returns new reverse list + index offset

(declare take-upto list-prefix? sublist-index)

(defn compact-block
  [block reverse-base]
  (let [block-size (count block)
        base-block (seq (reverse (take-upto reverse-base block-size)))
        base-block-size (count base-block)]
    (loop [base-block base-block
           offset 0]
      (if (list-prefix? base-block block)
        [(concat (reverse (nthrest block (- base-block-size offset)))
                 reverse-base)
         offset]
        (recur (rest base-block) (+ 1 offset))))))

; GET-VALUE is a thunk that returns the next value of the input vector
; every time it gets called.  BLOCK-SIZE is the size of the blocks in
; the algorithm.

; The procedure returns two values: the indices int array and an array of
; the actual values.

(defn compute-compact-table
  [get-value block-size]
  
  (let [get-block
        (fn []
          (loop [i 0 
                 block []]
            (if (>= i block-size) 
              block
              (if-let [value (get-value)]
                         (recur (+ 1 i) (conj block value))
                         block))))]

    (loop [reverse-values '()
           reverse-indices '()
           last-index 0
           ;; cache for blocks that have already shown up twice
           ;; (reduces run time *a lot*)
           bingo-block-set {}]

      (let [block (get-block)]
        (if (empty? block)
          [(int-array (reverse reverse-indices))
           (vec (reverse reverse-values))]
          (if-let [index (get bingo-block-set block)]
            (recur reverse-values
                   (cons index reverse-indices)
                   last-index
                   bingo-block-set)
            (if-let [rev-index (sublist-index (reverse block) reverse-values)]
              (recur reverse-values
                     (cons (+ (- block-size (count block)) (- last-index rev-index))
                           reverse-indices)
                     last-index
                     (assoc bingo-block-set
                       block (- last-index rev-index)))
              (let [[reverse-values offset] (compact-block block reverse-values)]
                (recur reverse-values
                       (cons (+ last-index offset) reverse-indices)
                       (+ last-index offset)
                       bingo-block-set)))))))))

; List utilities

(defn sublist-index
  [sublist list]
  (loop [list list
         index 0]
    (cond
     (list-prefix? sublist list) index
     (empty? list) nil
     :else (recur (rest list) (+ 1 index)))))

(defn list-prefix?
  [list-1 list-2]
  (loop [list-1 (seq list-1)
         list-2 (seq list-2)]
    (cond
     (empty? list-1) true
     (empty? list-2) false
     
     (= (first list-1) (first list-2))
     (recur (rest list-1) (rest list-2))
     
     :else false)))

(defn take-upto
  [list count]
  (loop [list (seq list)
         count count
         rev-result '()]
    (if (or (zero? count)
	    (empty? list))
      (reverse rev-result)
      (recur (rest list) (- count 1) (cons (first list) rev-result)))))

